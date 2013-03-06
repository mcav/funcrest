(ns stream.server
  (:use compojure.core)
  (:require [compojure.route :as route]
            [clojure.string :as str]
            [clojure.stacktrace :as stacktrace]
            [ring.util.codec :as codec]
            [stream.core :as core]))

(def ^:dynamic *req*)

;; stream.server/echo?foo=1,bar=2&
;; stream/server?data:application/json;....

(defn read-component [s]
  (-> s
      codec/form-decode
      read-string))

(defn qs->arg [s]
  (let [items (str/split s #";")]
    (if (= (count items) 1)
      (read-component (first items))
      (if (not= (.indexOf (first items) "=") -1)
        (into {} (map #(map read-component
                            (str/split % #"=" 2)) items))
        (map read-component items)))))

(defn query-string-to-args [qs]
  (if qs
    (read-component (second (first (re-seq #"args=([^&]+)" qs))))
    ;; (->> (str/split qs #"[&,]")
    ;;      (map qs->arg))
    nil))

(defn rpc-handler [req]
  (let [path (or (:path-info req) (:uri req))
        ns-and-fn (str/replace path #"^\/" "")
        func (core/find-accessible-func ns-and-fn)
        args (query-string-to-args (:query-string req))]
    (try
      (if func
        (binding [*req* req]
          (prn (apply list func args))
          {:status 200
           :headers {"content-type" "text/plain"}
           :body (pr-str (apply func args))})
        {:status 404
         :body (str "Function " ns-and-fn " not found.")})
      (catch Exception e
        {:status 500
         :headers {"content-type" "text/plain"}
         :body (with-out-str (stacktrace/print-stack-trace e))}))))

;; query arguments: ?foo=bar
;; body: {"foo": "bar"}

(defn print-request []
  *req*)

(defn echo [& args]
  (apply list 'echo args))

(defn write [s]
  (core/write s))

(defn cross-domain [rsp]
  (-> rsp
      (assoc-in [:headers "Access-Control-Allow-Origin"] "*")
      (assoc-in [:headers "Access-Control-Allow-Methods"]
                "GET, POST, PUT, DELETE, HEAD, OPTIONS")
      (assoc-in [:headers "Access-Control-Allow-Headers"]
                "X-Requested-With, Authorization, Content-Encoding, Content-Type")))

(defroutes app
  (ANY "/*" req (cross-domain (rpc-handler req)))
  (route/not-found "<h1>Page not found</h1>"))

;; {:ssl-client-cert nil,
;;  :remote-addr "127.0.0.1",
;;  :scheme :http,
;;  :request-method :get,
;;  :query-string "name=foo,bar",
;;  :route-params {:* "stream.server/print-request"},
;;  :content-type nil,
;;  :uri "/stream.server/print-request",
;;  :server-name "localhost",
;;  :params {:* "stream.server/print-request"},
;;  :headers
;;  {"user-agent"
;;   "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.21+ (KHTML, like Gecko) Version/6.0.2 Safari/536.26.17",
;;   "accept"
;;   "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
;;   "host" "localhost:3000",
;;   "accept-encoding" "gzip, deflate",
;;   "accept-language" "en-us",
;;   "connection" "keep-alive"},
;;  :content-length nil,
;;  :server-port 3000,
;;  :character-encoding nil,
;;  :body nil}
