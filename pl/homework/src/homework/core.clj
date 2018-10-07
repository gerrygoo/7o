
(ns homework.core (:gen-class))
(use 'clojure.tools.trace)
(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn -main []


    (defn aprox=
    "Checks if x is approximately equal to y. Returns true
    if |x - y| < epsilon, or false otherwise."
    [epsilon x y]
    (< (abs (- x y)) epsilon))


    (defn my-map-indexed
        ([f lst] (my-map-indexed f lst 0))
        ([f lst c] (if (empty? lst) () (cons (f c (first lst) ) (my-map-indexed f (rest lst) (inc c) ) ) ))
    )


    (deftest test-my-map-indexed
        (is (= () (my-map-indexed vector ())))
        (is (= '([0 a] [1 b] [2 c] [3 d])
            (my-map-indexed vector '(a b c d))))
        (is (= '(10 4 -2 8 5 5 13)
            (my-map-indexed + '(10 3 -4 5 1 0 7))))
        (is (= '(0 1 -4 3 1 0 6)
            (my-map-indexed min '(10 3 -4 5 1 0 7)))))

    (run-tests 'homework.core)
)
