;----------------------------------------------------------
  ; Activity: Problem Set: Recursive Functions II
  ; Date: September 2, 2018.
  ; Author:
  ;          A01371872 Gerardo Galván Olvera
  ;----------------------------------------------------------

(use 'clojure.test)

(defn my-repeat "The function my-repeat takes a number n and any data x as its arguments. It returns a list that contains n copies of x."
    [n x]
    (if (< n 1) () (cons x (my-repeat (dec n) x)) )
)

(defn invert-pairs "The function invert-pairs takes as an argument a list of vectors containing two elements each. It returns a new list with every vector pair inverted."
    [vector_list]
    (if (empty? vector_list)
        ()
        (cons (reverse (first vector_list)) (invert-pairs (rest vector_list))  )
    )
)

(defn enlist "The function enlist surrounds in a list every upper-level element of the list it takes as input."
    [l]
    (if (empty? l) () (cons (list (first l))  (enlist (rest l))) )
)

(defn my-interleave "The function my-interleave takes two arguments: the lists a and b. It returns a list containing the first element of a, followed by the first element of b, followed by the second element of a, followed by the second element of b, and so on."
    [a b]
    (cond
        (or (empty? a) (empty? b) ) ()
        :else (cons (first a) (cons (first b) (my-interleave (rest a) (rest b) ) ) )
    )
)

(defn my-flatten "The function my-flatten removes all the interior parenthesis of the list it takes as input."
    [l]
    (if (empty? l)
        ()
        (if (list? (first l))
            (concat (my-flatten (first l)) (my-flatten (rest l)) )
            (cons (first l) (my-flatten (rest l)))
        )
    )
)

(defn exchange "The function exchange takes three arguments: two non-list values x1 and x2, and a list lst. It returns a list with the same elements as lst, except that all occurrences of x1 are replaced by x2 and vice versa, including any occurrences inside nested lists."
    [x1 x2 lst]
    (if (empty? lst)
        ()
        (cons
            (cond
                (list? (first lst)) (exchange x1 x2 (first lst))
                (= (first lst) x1) x2
                (= (first lst) x2) x1
                :else (first lst)
            )
            (exchange x1 x2 (rest lst))
        )
    )
)

(defn insert "The function insert takes two arguments: a number n and a list of numbers lst in ascending order. It returns a new list with the same elements as lst but inserting n in its corresponding place."
    [n lst]
    (if (empty? lst)
        (list n)
        (if (< n (first lst))
            (cons n (cons (first lst) (rest lst)))
            (cons (first lst) (insert n (rest lst)))
        )
    )
)

(defn my-sort "The function my-sort takes an unordered list of numbers as an argument, and returns a new list with the same elements but in ascending order. You must use the insert function defined in the previous exercise to write the my-sort."
    [🌧]
    (if (empty? 🌧) () (insert (first 🌧) (my-sort (rest 🌧)) ) )
)

(defn binary "The function binary takes an integer n as input (assume that n ≥ 0). If n is equal to zero, it returns an empty list. If n is greater than zero, it returns a list with a sequence of ones and zeros equivalent to the binary representation of n"
    [n]
    (if (= n 0) () (concat (binary (quot n 2)) [(rem n 2)] ) )
)

(defn prime-factors "The function prime-factors takes an integer n as input (assume that n > 0), and returns a list containing the prime factors of n in ascending order."
  ([n] (prime-factors n 2) )
  ([n x]
  (if (< n 2) () (if (= (rem n x) 0) (cons x  (prime-factors (quot n x) x ) ) (prime-factors n (inc x)) )) )
)

(defn compress "The function compress takes a list lst as its argument. If lst contains consecutive repeated elements, they should be replaced with a single copy of the element."
    [l]
    (if (empty? l)
        ()
        (if (= (first l) (first (rest l)) )
            (compress (rest l))
            (cons (first l) (compress (rest l)) )
        )
    )
)

(defn lookslikelist? [e] (or (list? e) (seq? e) ))

(defn pack "The function pack takes a list lst as its argument. If lst contains consecutive repeated elements they should be placed in separate sublists."
    [l]
    (cond
        (empty? l) ()
        (lookslikelist? (last l)) l
        (not (lookslikelist? (first l))) (pack (cons (list (first l)) (rest l) ) )
        (= (last (first l)) (first (rest l)) ) (pack (cons (cons (first (rest l)) (first l) ) (rest(rest l)) ) )
        :else (cons (first l) (pack (rest l)) )
    )
)

(defn encode "The function encode takes a list lst as its argument. Consecutive duplicates of elements in lst are encoded as vectors [n e], where n is the number of duplicates of the element e."
  [l]
  (cond
    (empty? l) ()
    (vector? (last l)) l
    (not (vector? (first l))) (encode (cons [1 (first l)] (rest l) ))
    (= (last (first l)) (first (rest l)) ) (encode (cons [(inc (first (first l))) (last (first l))] (rest (rest l)) ) )
    :else (cons (first l) (encode (rest l)))
  )
)

(defn rRest [l] (rest(rest l)) )
(defn fFirst [a] (first (first a)))

(defn encode-modified "The function encode-modified takes a list lst as its argument. It works the same as the previous problem, but if an element has no duplicates it is simply copied into the result list."
  [l]
  (cond
    (< (count l) 2) l
    (not (vector? (first l)))
      (if (= (first l) (second l))
        (encode-modified (cons [2 (first l)] (rRest l) ))
        (cons (first l) (encode-modified (rest l)) )
      )
    :else
      (if (= (second (first l)) (second l))
        (encode-modified (cons [(inc (fFirst l)) (second l)] (rRest l) ))
        (cons (first l) (encode-modified (rest l)) )
      )
  )
)

(defn decode "The function decode takes as its argument an encoded list lst that has the same structure as the resulting list from the previous problem. It returns the decoded version of lst."
  [l]
  (if (empty? l)
    ()
    (if (vector? (first l))
      (if (> (fFirst l) 1)
        (decode (cons [ (dec (fFirst l)) (second (first l)) ] (cons (second (first l)) (rest l)) ))
        (cons (second (first l)) (decode (rest l)) )
      )
      (cons (first l) (decode (rest l)))
    )
  )
)

(deftest test-my-repeat
  (is (= () (my-repeat 0 'x)))
  (is (= '(6 6 6) (my-repeat 3 6)))
  (is (= '((ha ha) (ha ha) (ha ha)) (my-repeat 3 '(ha ha))))
  (is (= '(true true true true true) (my-repeat 5 true))))

(deftest test-invert-pairs
    (is (= () (invert-pairs ())))
    (is (= '([1 a][2 a][1 b][2 b]))
            (invert-pairs '([a 1][a 2][b 1][b 2])))
    (is (= '([1 January][2 February][3 March])
            (invert-pairs '([January 1][February 2][March 3])))))

(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
        (enlist '((1 2 3) 4 (5) 7 8)))))

(deftest test-my-interleave
  (is (= () (my-interleave () ())))
  (is (= () (my-interleave '(a) ())))
  (is (= () (my-interleave () '(1))))
  (is (= '(a 1 b 2 c 3 d 4 e 5)
          (my-interleave '(a b c d e) '(1 2 3 4 5))))
  (is (= '(a 1 b 2 c 3 d 4)
        (my-interleave '(a b c d e) '(1 2 3 4))))
  (is (= '(a 1 b 2 c 3 d 4)
        (my-interleave '(a b c d) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a) '(1 2 3 4 5))))
  (is (= '(a 1) (my-interleave '(a b c d e) '(1)))))

(deftest test-my-flatten
  (is (= () (my-flatten ())))
  (is (= '(a b c d e) (my-flatten '((a b) ((c) d (e))))))
  (is (= '(one two three four)
        (my-flatten '(((one) ((two))) () (three (())) four)))))

(deftest test-exchange
  (is (= () (exchange 'x 'y ())))
  (is (= '(d b c a) (exchange 'a 'd '(a b c d))))
  (is (= '((42) true ((cool (true)) (42))))
        (exchange true 42 '((true) 42 ((cool (42)) (true))))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-my-sort
  (is (= () (my-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9) (my-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (my-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (my-sort '(5 5 5 1 5 5 5)))))

(deftest test-binary
    (is (= () (binary 0)))
    (is (= '(1 1 1 1 0) (binary 30)))
    (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
        (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

(deftest test-pack
    (is (= () (pack ())))
    (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
          (pack '(a a a a b c c a a d e e e e))))
    (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
    (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

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
  (is (= '(a a a a b c c a a d e e e e)
        (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))

(run-tests)
; perfect docstrings taken from the source of the problems: http://webcem01.cem.itesm.mx:8005/apps/s201813/tc2006/programming_recursive_functions_II/