(ns demo.core
  (:use tupelo.core)
  (:require [clojure.pprint :as pprint]))

(defn exception-stacktrace-elems
  "Given  an exception, returns a vector of stacktrace elements."
  [exception]
  (into [] (.getStackTrace exception)))

(defmacro current-source-ns
  "Returns a string containing the namespace where this macro is called"
  [] '(try
        (throw (ex-info "throwing" {}))
        (catch Exception ex
          (it-> ex
            (exception-stacktrace-elems it)
            (first it)
            (.getClassName it)
            (re-find #"[.\w]+" it)))))


