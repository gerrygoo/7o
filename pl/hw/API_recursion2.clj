(use 'clojure.test)


(defn encode
    [l]
      (map
          (fn [sub] [(count sub) (first sub)] )
          (partition-by identity l)
      )
  )


(defn encode-modified
  [l]
    (map
        (fn [sub] (if (> (count sub) 1) [(count sub) (first sub)] (first sub) ) )
        (partition-by identity l)
    )
)

(defn decode
    [l]
    (mapcat (fn [sub] (if (vector? sub) (repeat (first sub) (second sub)) (list sub)) ) l)
)


(deftest test-encode
    (is (= () (encode ())))
    (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
           (encode '(a a a a b c c a a d e e e e))))
    (is (= '([1 1] [1 2] [1 3] [1 4] [1 5]) (encode '(1 2 3 4 5))))
    (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
        (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

  (deftest test-decode
    (is (= () (decode ())))
    (is (= '((a) (a) (a) (b) (b))
           (decode '([3 (a)] [2 (b)]))))
    (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
    (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)