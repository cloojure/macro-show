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


(dotest
  (is= "tst.demo.core" (spyx (current-source-ns)))
  )

(defn macro-show-impl
  [macro-expr]
  (let [macro-sym       (first macro-expr)
        macro-sym-fq    `~macro-sym
        macro-impl-sym  (str->sym (str (current-source-ns) "/"
                                    (sym->str macro-sym) "-impl"))
        macro-args      (rest macro-expr)
        macro-impl-call (list macro-impl-sym `(quote ~(vec macro-args)))
        macro-result    (eval macro-impl-call)]
    (spyx macro-impl-sym)
    (spyx macro-sym-fq)
    (spyx macro-impl-call)
    (spyx macro-result)
    macro-result))

(defmacro macro-show
  [macro-expr]
  `(pprint/pprint
    (quote ~(macro-show-impl macro-expr))))


(dotest
  (nl) (println :macro-show-impl-output-begin)
  (pretty (macro-show-impl '(unless false (println "forms!") :yes)))
  (println :macro-show-impl-output-done)
  (nl)
  (println :macro-show-begin)
  (macro-show
    (unless false (println "forms!") :yeeessssss!))
  (println :macro-show-done)

  (nl)
  (println "calling...")
  (is (unless false
        (println "forms!") :yeeessssss!))
  (println "  ...done")
  )




