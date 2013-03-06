;; Copyright 2013 Marcus Cavanaugh <m@mcav.com>
;; MIT Licensed.
(ns stream.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clj-http.client :as http])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer DataFlavor]))

(def root-dir (str (System/getProperty "user.dir") "/data"))

(defn timestamp-str []
  (.format (doto (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss'Z'")
             (.setTimeZone (java.util.TimeZone/getTimeZone "UTC")))
           (java.util.Date.)))

(defn mime-type-for-extension [filename]
  (.getContentType (javax.activation.MimetypesFileTypeMap.) filename))

(defn write [ext data]
  (let [key (timestamp-str)
        dest (io/file root-dir (str key "." ext))]
    (println "writing" (mime-type-for-extension (str "." ext)) key)
    (.mkdirs (io/file root-dir))
    (if (= (class data) java.io.File)
      (io/copy data dest)
      (spit dest data :encoding "UTF-8"))))

(defn write-clipboard []
  (let [transfer (-> (Toolkit/getDefaultToolkit)
                     (.getSystemClipboard)
                     (.getContents nil))
        flavors (vec (.getTransferDataFlavors transfer))
        files (some #{DataFlavor/javaFileListFlavor} flavors)
        text (some #{DataFlavor/stringFlavor} flavors)
        data (.getTransferData transfer (or files text))]
    (cond
     files (doall (for [f data]
                    (let [ext (subs (str f) (inc (.lastIndexOf (str f) ".")))]
                      (write ext f))))
     text (write "txt" data))))

(defn write-text [s]
  (write "txt" s))

(def w write-text)

(defn find-func [ns-and-fn]
  ;; find by "foo.bar.func" or "foo.bar/func"...
  (let [[namespace fn] (if (= -1 (.indexOf ns-and-fn "/"))
                         (let [chunks (str/split ns-and-fn #"\.")]
                           (if (> (count chunks) 1)
                             [(str/join "." (butlast chunks)) (last chunks)]
                             [ns-and-fn nil]))
                         (str/split ns-and-fn #"\/" 2))]
    (println "searching for" namespace fn)
    (find-var (if fn
                (symbol namespace fn)
                (symbol "clojure.core" namespace)))))

;; TODO: exclude private funcs?
(defn find-accessible-func [ns-and-fn]
  (let [func (find-func ns-and-fn)]
    (when (and func (ifn? func))
      (if (ifn? func)
        func
        (fn [& args] func)))))

(def endpoint "http://localhost:3000")
(defn client [method & args]
  (println "calling" method)
  (let [rsp (http/get (str endpoint "/" method)
                      {:query-params {:args (pr-str args)}})]
    (read-string (:body rsp))))

(defmacro rpc [& forms]
  `(do
     ~@(doall (for [[f# & args#] forms]
                `(client ~(str f#) ~@args#)))))