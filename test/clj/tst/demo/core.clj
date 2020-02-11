(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pprint]
    [tupelo.string :as ts]))

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

  (when false ; for debuggin to see result of macro expansion
    (nl) (println :macro-show-impl-output-begin)
    (pretty (macro-show-impl '(unless false (println "forms!") :yes)))
    (println :macro-show-impl-output-done))

  ; (nl) (println :macro-show-begin)
  (let [result-str (with-out-str
                     (macro-show
                       (unless false (println "forms!") :yeeessssss!)))]
    (is-nonblank= result-str
      (ts/quotes->double
        "(if
           (clojure.core/and true false)
           nil
           (do
             (println 'forms!')
             :yeeessssss!))")))
  ; (println :macro-show-done)

  (with-out-str ; suppress output during test run
    (nl) (println "calling...")
    (is= :yeeessssss! ; verify the macro actually works!
      (unless false
        (println "forms!")
        :yeeessssss!))
    (println "  ...done") )
  (is-nonblank= "forms!"
    (with-out-str
      (is= :yeeessssss!
        (unless false
          (println "forms!") :yeeessssss!))))

  )




