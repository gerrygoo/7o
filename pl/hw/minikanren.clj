;----------------------------------------------------------
; Problem Set: MiniKanren
; Date: November 25, 2018.
; Author:
;          A01371872 Gerardo Galván
;          A01371240 Ivan Varela
;----------------------------------------------------------


(use 'clojure.test)
(require '[clojure.core.logic :as l])
(require '[clojure.core.logic.fd :as fd])

(l/defne removeo "Succeeds if it’s able to remove the first occurrence of x from lst giving result."
    [ x lst result ]
    ( [ x [ head . tail ] tail ]
        (l/== x head))
    ( [ x [ head . tail ] result ]
        (l/fresh [temp]
            (removeo x tail temp)
            (l/conso head temp result))))


(l/defne reverseo"Logical function that succeeds if the the reverse of lst is result."
  [lst result]
  ([[] []])
  ([[head . tail] result]
   (l/fresh [temp]
     (l/appendo temp [head] result)
     (reverseo tail temp))))

(l/defne palindromeo "Succeeds if lst is a palindrome."
    [ lst ]
    ( [ lst ]
        (l/fresh [ reverse ]
        (l/== lst reverse)
        (reverseo lst reverse))))

(l/defne rotateo "Succeeds when lst is rotated left one position giving result."
    [ lst result ]
    ([ [ head . tail ] result ]
        (l/appendo tail [ head ] result)))

(declare oddsizeo)

(l/defne evensizeo "Succeeds if lst has an even size"
    [ lst ]
    ( [ [ ] ] )
    ( [ [ head . tail ] ](oddsizeo tail)))

(l/defne oddsizeo "Succeeds if lst has an odd size"
    [ lst ]
    ( [ [ head . tail ] ] (evensizeo tail)))

(l/defne splito
    "Succeeds when splitting lst gives a and b. The first, third, fifth, etc. elements of lst go to a, while the second, fourth, sixth, etc. elements go to b."
    [lst a b]
    ([ [] [] [] ])
    ([ [ head . tail ] a b ]
        (l/fresh [ ftail fb ]
            (l/firsto a head)
            (l/firsto tail ftail)
            (l/firsto b fb)
            (l/== ftail fb))))

(l/defne equalo "This logic function succeeds if all the elements contained in lst unify to the same value, otherwise fails."
    [lst]
    ([ [ ] ])
    ([ [ x ] ])
    ([ [ head . tail ] ]
        (l/fresh [ tail-head ]
            (l/firsto tail tail-head)
            (l/== head tail-head)
            (equalo tail))))

(l/defne counto "Unifies result with the number of elements contained in lst"
    [lst result]
    ([[ ] 0])
    ([[ head . tail ] result ]
        (l/fresh [ in-tail ]
            (fd/+ 1 in-tail result)
            (counto tail in-tail))))

(l/defne facto "This logic function succeeds if the factorial of n is equal to result."
    [n result]
    ([ 0 1 ])
    ([ n result ]
        (l/fresh [ prev prevfact ]
            (fd/- n 1 prev)
            (facto prev prevfact)
            (fd/* prevfact n result))))

(l/defne powo "This logic function succeeds if the factorial of n is equal to result."
    [base exp result]
    ([base 0 1])
    ([base 1 base])
    ([base exp result]
        (l/fresh [ prevexp prevpow ]
            (fd/in prevexp (fd/interval 0 100))
            (fd/in prevpow (fd/interval 0 100))
            (fd/- exp 1 prevexp)
            (powo base prevexp prevpow)
            (fd/* prevpow base result))))

(l/defne rangeo "This logic function unifies result with a sequence of incremental integers from start to end"
         [start end result]
         ([start end result]
           (l/fresh [temp next]
                    (fd/+ start 1 next)
                    (l/appendo [start] temp result)
                    (fd/< start end)
                    (rangeo next end temp)))
         ([start end result]
           (fd/> start end)
           (l/appendo [] [] result))
         ([end end result]
           (l/appendo [start] [] result)))

(deftest test-splito
  (is (= [:yes]
         (l/run 1 [q]
           (splito [] [] [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (splito [:a] [:a] [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (splito [:a :b] [:a] [:b])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (splito [:a :b :c :d :e :f]
                   [:a :c :e]
                   [:b :d :f])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (splito [:a :b :c :d :e :f :g]
                   [:a :c :e :g]
                   [:b :d :f])
           (l/== q :yes))))
  (is (= [[[:a :c :e] [:b :d :f]]]
         (l/run 1 [q1 q2]
           (splito [:a :b :c :d :e :f] q1 q2))))
  (is (= [[:a :b :c :d :e :f :g]]
         (l/run 1 [q]
           (splito q [:a :c :e :g] [:b :d :f]))))
  (is (= '[[[] [] []]
           [[_0] [_0] []]
           [[_0 _1] [_0] [_1]]
           [[_0 _1 _2] [_0 _2] [_1]]
           [[_0 _1 _2 _3] [_0 _2] [_1 _3]]
           [[_0 _1 _2 _3 _4] [_0 _2 _4] [_1 _3]]
           [[_0 _1 _2 _3 _4 _5] [_0 _2 _4] [_1 _3 _5]]]
         (l/run 7 [q1 q2 q3]
           (splito q1 q2 q3)))))

(deftest test-rangeo
  (is (= [[3 4 5 6 7 8 9 10]]
         (l/run 1 [q]
           (rangeo 3 10 q))))
  (is (= [[7]]
         (l/run 1 [q]
           (rangeo 7 7 q))))
  (is (= [[]]
         (l/run 1 [q]
           (rangeo 10 1 q))))
  (is (= [6]
         (l/run 1 [q]
           (fd/in q (fd/interval 1 10))
           (rangeo 2 q [2 3 4 5 6]))))
  (is (= [[2 6]]
         (l/run 1 [q1 q2]
           (fd/in q1 q2 (fd/interval 1 10))
           (rangeo q1 q2 [2 3 4 5 6]))))
  (is (= #{[]
           [1] [1 2] [1 2 3] [1 2 3 4]
           [2] [2 3] [2 3 4]
           [3] [3 4]
           [4]}
         (set
           (l/run* [q]
             (l/fresh [start end]
               (fd/in start end (fd/interval 1 4))
               (rangeo start end q)))))))

(deftest test-powo
  (is (= [:yes]
         (l/run 1 [q]
           (powo 3 2 9)
           (l/== q :yes))))
  (is (= [32]
         (l/run 1 [q]
           (powo 2 5 q))))
  (is (= [5]
         (l/run 1 [q]
           (powo q 2 25))))
  (is (= [3]
         (l/run 1 [q]
           (powo 2 q 8))))
  (is (= [1]
         (l/run 1 [q]
           (powo q q q))))
  (is (= #{[64 1] [8 2] [4 3] [2 6]}
         (set
           (l/run* [a b]
             (powo a b 64)))))
  (is (= '[_0]
         (l/run 1 [q]
           (powo q 0 1))))
  (is (= (set (range 101))
         (set
           (l/run* [q]
             (fd/in q (fd/interval 0 100))
             (powo q 1 q))))))

(deftest test-facto
  (is (= [1]
         (l/run 1 [q]
           (facto 0 q))))
  (is (= [1]
         (l/run 1 [q]
           (facto 1 q))))
  (is (= [720]
         (l/run 1 [q]
           (facto 6 q))))
  (is (= [2432902008176640000]
         (l/run 1 [q]
           (facto 20 q))))
  (is (= [0 1]
         (l/run 2 [q]
           (facto q 1))))
  (is (= [5]
         (l/run 1 [q]
           (facto q 120))))
  (is (= [10]
         (l/run 1 [q]
           (facto q 3628800))))
  (is (= [:yes]
         (l/run 1 [q]
           (facto 4 24)
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (facto 15 1307674368000)
           (l/== q :yes))))
  (is (= [[0 1]
          [1 1]
          [2 2]
          [3 6]
          [4 24]
          [5 120]
          [6 720]
          [7 5040]
          [8 40320]
          [9 362880]]
         (l/run 10 [n r]
           (facto n r)))))

(deftest test-equalo
  (is (= [:yes]
         (l/run 1 [q]
           (equalo [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (equalo [:x])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (equalo [:x :x])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (equalo [:x :x :x :x :x])
           (l/== q :yes))))
  (is (= [:x]
         (l/run 1 [q]
           (equalo [:x :x q :x]))))
  (is (= '[_0]
         (l/run 1 [q]
           (equalo [q q q q q q]))))
  (is (= '([_0 _0 _0 _0 _0])
         (l/run 1 [q1 q2 q3 q4 q5]
           (equalo [q1 q2 q3 q4 q5]))))
  (is (= []
         (l/run 1 [q]
           (equalo [:x :y])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q1 q2]
           (equalo [q1 q1 q2 q1 q1])
           (l/!= q1 q2))))
  (is (= '([]
           [_0]
           [_0 _0]
           [_0 _0 _0]
           [_0 _0 _0 _0]
           [_0 _0 _0 _0 _0]
           [_0 _0 _0 _0 _0 _0])
         (l/run 7 [q]
           (equalo q)))))

(deftest test-counto
  (is (= [0]
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto [] q))))
  (is (= [1]
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto [:a] q))))
  (is (= [2]
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto [:a :b] q))))
  (is (= [3]
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto [:a :b :c] q))))
  (is (= [10]
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto (repeat 10 :x) q))))
  (is (= '([_0])
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto q 1))))
  (is (= '([_0 _1 _2 _3 _4])
         (l/run 1 [q]
           (fd/in q (fd/interval 0 10))
           (counto q 5))))
  (is (= '([[] 0]
           [(_0) 1]
           [(_0 _1) 2]
           [(_0 _1 _2) 3]
           [(_0 _1 _2 _3) 4]
           [(_0 _1 _2 _3 _4) 5]
           [(_0 _1 _2 _3 _4 _5) 6])
         (l/run 7 [q1 q2]
           (fd/in q1 q2 (fd/interval 0 10))
           (counto q1 q2)))))

(deftest test-evensizeo-oddsizeo
  (is (= [:yes]
         (l/run 1 [q]
           (evensizeo [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (oddsizeo [:x])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (evensizeo [:x])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (oddsizeo [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (evensizeo [:a :b :c :d :e :f])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (oddsizeo [:a :b :c :d :e])
           (l/== q :yes))))
  (is (= '[[]
           [_0 _1]
           [_0 _1 _2 _3]
           [_0 _1 _2 _3 _4 _5]
           [_0 _1 _2 _3 _4 _5 _6 _7]]
         (l/run 5 [q]
           (evensizeo q))))
  (is (= '[[_0]
           [_0 _1 _2]
           [_0 _1 _2 _3 _4]
           [_0 _1 _2 _3 _4 _5 _6]
           [_0 _1 _2 _3 _4 _5 _6 _7 _8]]
         (l/run 5 [q]
           (oddsizeo q)))))

(deftest test-rotateo
  (is (= [:yes]
         (l/run 1 [q]
           (rotateo [:a :b :c :d :e]
                    [:b :c :d :e :a])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (rotateo [:a :b :c :d :e]
                    [:a :b :c :d :e])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (rotateo [] q))))
  (is (= [[:a]]
         (l/run 1 [q]
           (rotateo [:a] q))))
  (is (= [[:b :c :d :e :a]]
         (l/run 1 [q]
           (rotateo [:a :b :c :d :e] q))))
  (is (= [[:e :a :b :c :d]]
         (l/run 1 [q]
           (rotateo q [:a :b :c :d :e]))))
  (is (= '[[[_0] [_0]]
           [[_0 _1] [_1 _0]]
           [[_0 _1 _2] [_1 _2 _0]]
           [[_0 _1 _2 _3] [_1 _2 _3 _0]]
           [[_0 _1 _2 _3 _4] [_1 _2 _3 _4 _0]]
           [[_0 _1 _2 _3 _4 _5] [_1 _2 _3 _4 _5 _0]]
           [[_0 _1 _2 _3 _4 _5 _6] [_1 _2 _3 _4 _5 _6 _0]]]
         (l/run 7 [q1 q2]
           (rotateo q1 q2)))))

(deftest test-palindromeo
  (is (= [:yes]
         (l/run 1 [q]
           (palindromeo [])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (palindromeo [:a])
           (l/== q :yes))))
  (is (= [:yes]
         (l/run 1 [q]
           (palindromeo [:a :b :c :b :a])
           (l/== q :yes))))
  (is (= []
         (l/run 1 [q]
           (palindromeo [:a :b :c :d])
           (l/== q :yes))))
  (is (= '[[]
           [_0]
           [_0 _0]
           [_0 _1 _0]
           [_0 _1 _1 _0]
           [_0 _1 _2 _1 _0]
           [_0 _1 _2 _2 _1 _0]]
         (l/run 7 [q]
           (palindromeo q)))))

(deftest test-removeo
  (is (= [[:b :c :d :e]]
         (l/run 1 [q]
           (removeo :a [:a :b :c :d :e] q))))
  (is (= [[:a :b :d :e]]
         (l/run 1 [q]
           (removeo :c [:a :b :c :d :e] q))))
  (is (= [:d]
         (l/run 1 [q]
           (removeo q [:a :b :c :d :e] [:a :b :c :e]))))
  (is (= []
         (l/run 1 [q]
           (removeo :x [:a :b :c :d :e] q))))
  (is (= [[:x :a :b :c :d :e]
          [:a :x :b :c :d :e]
          [:a :b :x :c :d :e]
          [:a :b :c :x :d :e]
          [:a :b :c :d :x :e]
          [:a :b :c :d :e :x]]
         (l/run 6 [q]
           (removeo :x q [:a :b :c :d :e]))))
  (is (= [[:a [:b :c :d :e]]
          [:b [:a :c :d :e]]
          [:c [:a :b :d :e]]
          [:d [:a :b :c :e]]
          [:e [:a :b :c :d]]]
         (l/run* [q1 q2]
           (removeo q1 [:a :b :c :d :e] q2)))))

(run-tests)