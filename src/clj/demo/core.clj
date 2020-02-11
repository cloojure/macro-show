(ns demo.core
  (:use tupelo.core)
  (:require [clojure.pprint :as pprint]))

(defn exception-stacktrace-elems
  "Given  an exception, returns a vector of stacktrace elements."
  [exception]
  (vec (.getStackTrace exception)))

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

; #todo pull in this entire code block as a literal, then use eval in THIS ns!
(defn macro-show-impl
  [macro-expr]
  `(do
     ; (spyxx (quote ~macro-expr))
     (let [macro-sym#                (first (quote ~macro-expr))
           macro-args#               (rest (quote ~macro-expr))

           ; We cannot use `(current-source-ns)` since it reads THIS ns, not caller's ns,
           ; so we must inline it and return this whole code chunk to the caller's ns.
           macro-callsite-source-ns# (try
                                       (throw (Exception. "dummy throw"))
                                       (catch Exception ex#
                                         (->> ex#
                                           (exception-stacktrace-elems)
                                           (first)
                                           (.getClassName)
                                           (re-find #"[.\w]+"))))
           ; xx#                       (spyx macro-callsite-source-ns#)

           macro-impl-sym#           (str->sym (str macro-callsite-source-ns# "/"
                                                 (sym->str macro-sym#) "-impl"))
           ; xx#                       (spyx macro-impl-sym#)
           macro-impl-call#          (list macro-impl-sym# `(quote ~(vec macro-args#)))
           ; xx#                       (spyx macro-impl-call#)
           macro-result#             (->list (eval macro-impl-call#))
           ; xx#                       (spyxx macro-result#)
           ]
       (pprint/pprint macro-result#))))

(defmacro macro-show
  [macro-expr]
  (macro-show-impl macro-expr))


