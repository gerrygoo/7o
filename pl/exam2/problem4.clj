;==========================================================
; Type your student ID and name here.
;==========================================================

(use 'clojure.test)
(import 'clojure.lang.IFn)
(declare $eval)

;==========================================================
(deftype Closure
  [env params body]

  IFn

  (invoke
    [self args]
    ($eval body (merge @env (zipmap params args))))

  (applyTo
    [self args]
    (self args)))

;==========================================================
(defn third
  "Returns third element of lst."
  [lst]
  (nth lst 2))

;==========================================================
(defn fourth
  "Returns fourth element of lst."
  [lst]
  (nth lst 3))

;==========================================================
(defn $eval
  "Evaluate an expression expr in the context of
  an environment env and return the result."
  [expr env]
  (cond

    (symbol? expr) ; expr is a variable reference
    (if (contains? env expr)
      (get env expr)
      (throw (RuntimeException.
               (str "Unbound variable: " expr))))

    (list? expr) ; expr is a list, so keep checking what's in it
    (case (first expr)

      nil ; expr is an empty list
      ()

      quote ; expr is a "quote" special form
      (second expr)

      if ; expr is an "if" special form
      (if ($eval (second expr) env)
        ($eval (third expr) env)
        ($eval (fourth expr) env))

      lambda ; expr is a "lambda" special form
      (->Closure (atom env) (second expr) (third expr))

      label ; expr is a "label" special form
      (let [lambda-expr ($eval (third expr) env)]
        (swap! (.env lambda-expr)
               #(assoc % (second expr) lambda-expr))
        lambda-expr)

      ; (let (var expr) body)
      let
      ($eval
        (third expr)
        (assoc env (->> expr second first) ($eval (->> expr second second) env))
      )

      ; else, expr is a function invocation
      (apply ($eval (first expr) env)
             (map #($eval % env)
                  (rest expr))))


    ; expr is something else, so let it $eval to itself
    :else
    expr))

;==========================================================
(deftest test-var-ref
  (is (= 15 ($eval 'c
                      {'a 4, 'b 8, 'c 15})))
  (is (thrown? RuntimeException
               ($eval 'x
                         {'a 4, 'b 8, 'c 15}))))

;==========================================================
(deftest test-itself
  (is (= 42 ($eval 42 {})))
  (is (= true ($eval true {})))
  (is (= false ($eval false {})))
  (is (= nil ($eval nil {})))
  (is (= "hello" ($eval "hello" {}))))

;==========================================================
(deftest test-empty-list
  (is (= () ($eval () {}))))

;==========================================================
(deftest test-quote
  (is (= 'a
         ($eval '(quote a) {})))
  (is (= '(1 2 3)
         ($eval '(quote (1 2 3)) {})))
  (is (= '42 ($eval '(quote 42) {}))))

;==========================================================
(deftest test-if
  (is (= 1 ($eval '(if true 1 2) {})))
  (is (= 2 ($eval '(if false 1 2) {}))))

;==========================================================
(deftest test-function-invocation
  (is (= 3
         ($eval '(f 1 2)
                {'f +})))
  (is (= 'a
         ($eval '(g (quote (a b c d e)))
                {'g first})))
  (is (= '(a b c)
         ($eval '(cons x y)
                {'cons cons, 'x 'a, 'y '(b c)})))
  (is (= 55
         ($eval '(+ 1 2 3 4 5 6 7 8 9 10)
                {'+ +})))
  (is (= '(a b c)
         ($eval '(apply cons (quote (a (b c))))
                {'apply apply, 'cons cons}))))

;==========================================================
(deftest test-lambda
  (let [c ($eval '(lambda (x)
                    (* x 2))
                 {'* *})]
    (is (instance? Closure c))
    (is (= @(.env c) {'* *}))
    (is (= (.params c) '(x)))
    (is (= (.body c) '(* x 2)))
    (is (= 42 (c '(21))))
    (is (= 42 (apply c '(21)))))
  (is (= 8
         ($eval '((lambda (f x) (f (f (f x))))
                  (lambda (x) (* x 2))
                  1)
                 {'* *}))))

;==========================================================
(deftest test-label
  (is (= '(a a b b c c)
         ($eval
           '((label dup (lambda (lst)
                          (if (eq lst ())
                            ()
                            (cons (car lst)
                                  (cons (car lst)
                                        (dup (cdr lst)))))))
             (quote (a b c)))
           {'eq   =
            'cons cons
            'car  first
            'cdr  rest})))
  (is (= '(1 4 9 16)
         ($eval
           '((label mapcar (lambda (fun lst)
                             (if (eq lst ())
                               ()
                               (cons (fun (car lst))
                                     (mapcar fun (cdr lst))))))
             (lambda (x) (* x x))
             (quote (1 2 3 4)))
           {'eq   =
            'cons cons
            'car  first
            'cdr  rest
            '*    *}))))

;==========================================================
(deftest test-let
  (is (= 7
         ($eval '(let (a 7) a) {})))
  (is (= 42
         ($eval '(let (x 6)
                   (* 7 x))
                {'* *})))
  (is (= 111
         ($eval '(let (x (* 2 5))
                   (let (y (+ 1 x))
                     (+ 1 (* y x))))
                {'+ +
                 '* *})))
  (is (= 60
         ($eval '((lambda (x y)
                    (let (y (+ 1 y))
                      (* x y)))
                  10
                  5)
                {'* *
                 '+ +})))
  (is (= '(a b c d)
         ($eval '(let (one (quote (c d)))
                   (let (two (cons (quote b) one))
                     (let (three (cons (quote a) two))
                       three)))
                {'cons cons})))
  (is (= 8
         ($eval '(((lambda (x y)
                     (let (z (let (r) (* x y)) r)
                       (lambda (w)
                         (let (t (+ x (let (a y) a) z w))
                           (let (t t) t))))) 1 2) 3)
                {'* *
                 '+ +}))))

;==========================================================
(run-tests)