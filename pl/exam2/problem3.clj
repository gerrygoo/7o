;==========================================================
; Type your student ID and name here.
;==========================================================

(use 'clojure.test)
(use 'clojure.math.numeric-tower)

;==========================================================
(defn concat-repeat-num
  "Returns an infinite lazy sequence of integers (most of
  them BigInts) starting with n, then n concatenated twice,
  then n concatenated three times, and so on, to infinity
  and beyond."
  [n]
  (iterate
    #(+' (*' % (expt 10 (count (str n)) )) n)
    n
  )
)

;==========================================================
(deftest test-concat-repeat-num
  (is (= 9
         (first (concat-repeat-num 9))))
  (is (= '(2 22 222 2222 22222)
         (take 5 (concat-repeat-num 2))))
  (is (= '(123456
            123456123456
            123456123456123456
            123456123456123456123456N
            123456123456123456123456123456N)
         (take 5 (concat-repeat-num 123456))))

  (is (= '(0 0 0 0 0 0 0 0 0 0)
         (take 10 (concat-repeat-num 0))))

  (is (= '(1
            11
            111
            1111
            11111
            111111
            1111111
            11111111
            111111111
            1111111111)
         (take 10 (concat-repeat-num 1))))

  (is (= 666666666666666666666N
         (nth (concat-repeat-num 6) 20)))

  (is (= '(1234123412341234
            12341234123412341234N
            123412341234123412341234N)
         (take 3 (drop 3 (concat-repeat-num 1234)))))

  (is (= 36196
         (rem (nth (concat-repeat-num 32910741)
                   10000)
              61463)))
  )

;==========================================================
(run-tests)
