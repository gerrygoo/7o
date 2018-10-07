(use 'clojure.test)
(use 'clojure.math.numeric-tower)

(defn f2c [far] (* (- far 32) (/ 5 9) ) )

(defn myroot [a b c sign]
  (/
    (+
      (* b -1)
      (* sign (sqrt (- (* b b) (* 4 a c)))))
    (* 2 a)
    )
  )

(defn roots [a b c] (vector (myroot a b c 1) (myroot a b c -1) ) )

(defn bmi-judgement [w h]
  (let
    [ bmi (/ w (* h h) ) ]
    (cond
      (< bmi 20) 'underweight
      (and (>= bmi 20) (< bmi 25 ) ) 'normal
      (and (>= bmi 25) (< bmi 30 ) ) 'obese1
      (and (>= bmi 30) (< bmi 40 ) ) 'obese2
      :else 'obese3

      )
    )
  )

(defn sign [x] (cond  (< x 0) -1  (> x 0) 1 :else 0 ) )

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(deftest test-bmi
  (is (= 'underweight (bmi-judgement 45 1.7)))
  (is (= 'normal (bmi-judgement 55 1.5)))
  (is (= 'obese1 (bmi-judgement 76 1.7)))
  (is (= 'obese2 (bmi-judgement 81 1.6)))
  (is (= 'obese3 (bmi-judgement 120 1.6))))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(run-tests)