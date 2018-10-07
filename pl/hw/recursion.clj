;----------------------------------------------------------
; Activity: Problem Set: Recursive Functions I
; Date: August 26, 2018.
; Author:
;          A01371872 Gerardo Galvan
;----------------------------------------------------------

(use 'clojure.test)

(defn my-count "The function my-count returns the number of elements contained in its input list."
  [list]
  ( if (empty? list) 0 (+ 1 (my-count (rest list) ) ) )
  )

(defn add-list "The function add-list returns the sum of all the elements of its input list, or 0 if its empty."
  [list]
  ( if (empty? list) 0 (+ (first list) (add-list (rest list) ) ) )
  )

(defn member? "The function member? takes two arguments, any data x and a list lst. Returns true if x is contained in lst, false otherwise."
  [x list]
  (cond (empty? list) false (= x (first list)) true :else (member? x (rest list)) )
  )

(defn list-of-symbols? "The function list-of-symbols? takes a list lst as its argument. It returns true if all the elements (possibly zero) contained in lst are symbols, or false otherwise. "
  [list]
  (or (empty? list) (and (symbol? (first list)) (list-of-symbols? (rest list)) ) )
  )

(defn my-last "The function my-last returns the last element of its input list, or nil of its empty."
  [list] (if (empty? list) nil (if (empty?(rest list)) (first list) (my-last(rest list)) ) ))

(defn cons-end "The function cons-end takes two arguments, any data x and a list lst. Returns a list composed by the same elements of lst but with x at the end."
  [x mylist]
  (if (empty? mylist)
    (list x)
    (cons (first mylist) (cons-end x (rest mylist)))
    )
  )

(defn my-reverse "The function my-reverse takes a list as an argument. It returns another list with the same elements as the input list, but in reverse order. "
  [list]
  (if
    (< (count list) 2 )
    list
    (conj (concat (my-reverse  (rest (butlast list))) [(first list)] ) (last list) )
    )
  )

(defn my-butlast "The function my-butlast returns a list with the same elements as its input list but excluding the last element, or nil of its empty."
  [list]
  (if (empty? list)
    nil
    (if (empty? (rest list) )
      ()
      (cons (first list) (my-butlast (rest list)) )
      )
    )
  )

(defn my-concat "The function my-concat returns the resulting list of appending the two lists it takes as input."
  [a b]
  (cond
    (empty? b) a
    (empty? a) b
    :else (cons (first a) (my-concat (rest a) b) )
    )
  )

(defn deep-reverse "The function deep-reverse takes a list as its input. It returns a list with the same elements as its input but in reverse order. If there are any nested lists, these too are reversed."
  [l]
  (if (< (count l) 2)
    (if (list? (first l)) (list (deep-reverse (first l))) l )
    (cons
      (if (list? (last l)) (deep-reverse (last l)) (last l) )
      (cons-end 
        (if (list? (first l)) (deep-reverse (first l))  (first l) ) 
        (deep-reverse (rest (butlast l)) )
      )
    )
  )
)


(deftest test-my-count
  (is (= 0 (my-count ())))
  (is (= 1 (my-count '(a))))
  (is (= 3 (my-count '(a b c)))))

(deftest test-add-list
  (is (= 0 (add-list ())))
  (is (= 10 (add-list '(2 4 1 3))))
  (is (= 55 (add-list '(1 2 3 4 5 6 7 8 9 10)))))

(deftest test-member?
  (is (not (member? 'a ())))
  (is (member? 'a '(a b c)))
  (is (member? 'a '(c b a b c)))
  (is (not (member? 'x '(a b c)))))

(deftest test-list-of-symbols?
  (is (list-of-symbols? ()))
  (is (list-of-symbols? '(a)))
  (is (list-of-symbols? '(a b c d e)))
  (is (not (list-of-symbols? '(a b c d 42 e))))
  (is (not (list-of-symbols? '(42 a b c)))))

(deftest test-my-last
  (is (nil? (my-last ())))
  (is (= 'x (my-last '(x))))
  (is (= 'c (my-last '(a b c)))))

(deftest test-cons-end
  (is (= '(b c d a) (cons-end 'a '(b c d))))
  (is (= '(a) (cons-end 'a ()))))

(deftest test-my-reverse
  (is (= () (my-reverse ())))
  (is (= '(c b a) (my-reverse '(a b c))))
  (is (= '(3 (b c d) a) (my-reverse '(a (b c d) 3)))))

(deftest test-my-butlast
  (is (nil? (my-butlast ())))
  (is (= () (my-butlast '(x))))
  (is (= '(a b) (my-butlast '(a b c)))))

(deftest test-my-concat
  (is (= '(a b c) (my-concat '(a b c) ())))
  (is (= '(1 2 3) (my-concat () '(1 2 3))))
  (is (= '(a b c 1 2 3) (my-concat '(a b c) '(1 2 3)))))

(deftest test-deep-reverse
  (is (= () (deep-reverse ())))
  (is (= '(3 (d c b) a) (deep-reverse '(a (b c d) 3))))
  (is (= '(((6 5) 4) 3 (2 1))
         (deep-reverse '((1 2) 3 (4 (5 6)))))))

(run-tests)