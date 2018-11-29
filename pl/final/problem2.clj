;==========================================================
; Gerardo Galván Olvera A01371872
;==========================================================

(use 'clojure.test)

;==========================================================
(defmacro def-many
    "This macro allows defining many global bindings in one
    place. It has the following form:

    (def-many var1 expr1 var2 expr2 ... varn exprn)

    Where every vari is a symbol and every expri is an
    arbitrary expression. The macro evaluates expr1 and binds
    the result to var1 (using the def special form), then
    evaluates expr2 and binds the result to var2, and so on.

    The macro expands to the following form:

    (do
        (def var1 expr1)
        (def var2 expr2)
        ...
        (def varn exprn))"
    [& args]
    `(do ~@(map #(cons 'def %) (partition 2 args))))

;==========================================================
(deftest test-def-many
  (is (= '(do
            (def my-var (inc (* 2 5))))
         (macroexpand-1 '(def-many
                           my-var (inc (* 2 5))))))
  (is (= 11
         (do
           (def-many
             my-var (inc (* 2 5)))
           my-var)))
  (is (= '(do
            (def a (+ 1 2))
            (def b (* 2 a))
            (def c (/ (inc b) a)))
         (macroexpand-1 '(def-many
                           a (+ 1 2)
                           b (* 2 a)
                           c (/ (inc b) a)))))
  (is (= "3 6 7/3"
         (with-out-str
           (def-many
             a (+ 1 2)
             b (* 2 a)
             c (/ (inc b) a))
           (print a b c))))
  (is (= '(do
            (def x1 1)
            (def x2 2)
            (def x3 3)
            (def x4 4)
            (def x5 5)
            (def x6 6)
            (def x7 7)
            (def x8 8)
            (def x9 9)
            (def x10 10))
         (macroexpand-1 '(def-many
                           x1 1
                           x2 2
                           x3 3
                           x4 4
                           x5 5
                           x6 6
                           x7 7
                           x8 8
                           x9 9
                           x10 10))))
  (is (= "Result of adding everything = 0055"
         (with-out-str
           (def-many
             x1 1
             x2 2
             x3 3
             x4 4
             x5 5
             x6 6
             x7 7
             x8 8
             x9 9
             x10 10)
           (printf "Result of adding everything = %04d"
                   (+ x1 x2 x3 x4 x5 x6 x7 x8 x9 x10))))))

;==========================================================
(run-tests)
