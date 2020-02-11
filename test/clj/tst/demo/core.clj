(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pprint]))

; desired output format of...
(comment
  (defmacro+showable
    [pred & forms]
    `(if (and true ~pred)
       nil
       (do ~@forms))))
;...is the following:
;---------------------------------------------------------------------------------------------------
(do       ; *** live code ***
  (defn unless-impl
    [args]
    (let [[pred & forms] args]
      `(if (and true ~pred)
         nil
         (do ~@forms))))

  (defmacro unless
    [& args]
    (unless-impl args)))
;---------------------------------------------------------------------------------------------------


;(dotest
;  (is= "tst.demo.core" (spyx (current-source-ns)))
;  )

; #todo pull in this entire code block as a literal, then use eval in THIS ns!
(defn macro-show-impl
  [macro-expr]
 `(do
   ;(spyxx (quote ~macro-expr))
    (let [macro-sym#         (first (quote ~macro-expr))
          macro-args#        (rest (quote ~macro-expr))

          current-source-ns# (try
                               (throw (Exception. "dummy throw"))
                               (catch Exception ex#
                                 (->> ex#
                                   (exception-stacktrace-elems)
                                   (first)
                                   (.getClassName)
                                   (re-find #"[.\w]+"))))
        ; xxx#               (println :50 current-source-ns#)

          macro-impl-sym#    (str->sym (str current-source-ns# "/"
                                         (sym->str macro-sym#) "-impl"))
          macro-impl-call#   (list macro-impl-sym# `(quote ~(vec macro-args#)))
          macro-result#      (->list (eval macro-impl-call#))
          ]
      ;(spyx macro-impl-sym#)
      ;(spyx macro-impl-call#)
      ;(spyxx macro-result#)
      (pretty macro-result#)
      )))

(defmacro macro-show
  [macro-expr]
  (macro-show-impl macro-expr))

(dotest

  (nl) (println :macro-show-impl-output-begin)
  (pretty (macro-show-impl '(unless false (println "forms!") :yes)))
  (println :macro-show-impl-output-done)

  (nl) (println :macro-show-begin)
  (macro-show
    (unless false (println "forms!") :yeeessssss!))
  (println :macro-show-done)


  ;(nl)
  ;(println "calling...")
  ;(is (unless false
  ;      (println "forms!") :yeeessssss!))
  ;(println "  ...done")

  )




