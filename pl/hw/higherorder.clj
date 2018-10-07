
;----------------------------------------------------------
; Problem Set: Higher-Order Functions
; Date: September 9, 2018.
; Author:
;          A01371872 Gerardo Galván
;----------------------------------------------------------

(use 'clojure.math.numeric-tower)
(use 'clojure.test)

(defn aprox=
  "Checks if x is approximately equal to y. Returns true
  if |x - y| < epsilon, or false otherwise."
  [epsilon x y]
  (< (abs (- x y)) epsilon))

(defn my-map-indexed "The function my-map-indexed takes two arguments: a function f and a list lst. It returns a list consisting of the result of applying f to 0 and the first item of lst, followed by applying f to 1 and the second item in lst, and so on until lst is exhausted."
    ([f lst] (my-map-indexed f lst 0))
    ([f lst c] (if (empty? lst) () (cons (f c (first lst) ) (my-map-indexed f (rest lst) (inc c) ) ) ))
)

(defn my-drop-while "The function my-drop-while takes two arguments: a function f and a list lst. It returns a list of items from lst dropping the initial items that evaluate to true when passed to f. Once a false value is encountered, the rest of the list is returned."
    [f lst]
    (if (empty? lst) () (if (f (first lst)) (my-drop-while f (rest lst)) lst ) )
)

(defn signeq? [a b] "for a != b" (or (and (> a 0) (> b 0)) (and (< a 0) (< b 0))))

(defn bisection "The function bisection takes a, b, and f as arguments. It finds the corresponding root of f using the bisection method."
    [a b f]
    (let [m (+ a (/ (- b a) 2.0)) fm (f m) ]
        (if (aprox= 0.0001 fm 0.0 ) m (if (signeq? (f m) (f a)) (bisection m b f) (bisection a m f) ) )
    )
)

(defn deriv "The functionderiv takes f and h as its arguments, and returns a new function that takes x as argument, and which represents the derivate of f given a certain value for h."
    [f h]
    (fn [x] (/ (- (f (+ x h)) (f x) ) h) )
)

(defn integral "The function integra takes as arguments a, b, n, and f. It returns the value of the integral, using Simpson's rule."
    ([a b n f] (* (integral a b n n f) (/ (/ (- b a) n) 3) ) )
    ([a b n k f] (let [h (/ (- b a) n)]
        (if (= k 0)
            (f a)
            (+
                (* (cond (= k n) 1 (= (mod k 2) 0) 2 :else 4) (f (+ a (* k h))) )
                (integral a b n (dec k) f)
            )
        )
    ))
)

(deftest test-my-map-indexed
    (is (= () (my-map-indexed vector ())))
    (is (= '([0 a] [1 b] [2 c] [3 d])
           (my-map-indexed vector '(a b c d))))
    (is (= '(10 4 -2 8 5 5 13)
           (my-map-indexed + '(10 3 -4 5 1 0 7))))
    (is (= '(0 1 -4 3 1 0 6)
           (my-map-indexed min '(10 3 -4 5 1 0 7)))))

(deftest test-my-drop-while
    (is (= () (my-drop-while neg? ())))
    (is (= '(0 1 2 3 4)
            (my-drop-while
                neg?
                '(-10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4))))
    (is (= '(2 three 4 five)
            (my-drop-while
                symbol?
                '(zero one 2 three 4 five))))
    (is (= '(0 one 2 three 4 five)
            (my-drop-while
                symbol?
                '(0 one 2 three 4 five)))))

(deftest test-bisection
  (is (aprox= 0.0001
              3.0
              (bisection 1 4 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              -4.0
              (bisection -5 0 (fn [x] (* (- x 3) (+ x 4))))))
  (is (aprox= 0.0001
              Math/PI
              (bisection 1 4 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              (* 2 Math/PI)
              (bisection 5 10 (fn [x] (Math/sin x)))))
  (is (aprox= 0.0001
              1.618033988749895
              (bisection 1 2 (fn [x] (- (* x x) x 1)))))
  (is (aprox= 0.0001
              -0.6180339887498948
              (bisection -10 1 (fn [x] (- (* x x) x 1)))))
)

(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))
(deftest test-deriv
  (is (aprox= 0.05 75 (df 5)))
  (is (aprox= 0.05 30 (ddf 5)))
  (is (aprox= 0.05 6 (dddf 5))))

(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
           (fn [x]
             (integral 3 4 10
               (fn [y]
                 (* x y))))))))

(run-tests)
; perfect docstrings taken and inspired by the source of the problems: http://webcem01.cem.itesm.mx:8005/apps/s201813/tc2006/programming_higher_order/