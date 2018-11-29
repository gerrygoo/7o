;----------------------------------------------------------
; Activity: Problem Set: Macros
; Date: November 11, 2018.
; Authors:
;          A01371872 Gerardo Galván Olvera
;----------------------------------------------------------

(defmacro my-or "Evaluates its expressions one at a time, from left to right. If a form returns a logical true value, it returns that value and doesn't evaluate any of the other expressions, otherwise it returns the value of the last expression. (or) returns nil."
    ([] nil)
    ([x] x)
    ([x & rest]
        `(if ~x
            ~x
            (my-or ~@rest))))


(defmacro do-loop [& body] "the expressions in the body are evaluated sequentially, and then the condition is evaluated. If the final form uses a :while keyword, the body of the loop is repeated while the condition holds true. On the other hand, if the final form uses an :until keyword, the body of the loop is repeated while the condition holds false (or in other words, the loop terminates when the condition is true). Fails to return nil."
    `((defn ~'f [ ]
        (cond
            (or
                (and (= ~(first (last body)) :while) ~(last (last body)) )
                (and (= ~(first (last body)) :until) (not ~(last (last body))) )
            )
            (do ~@(butlast body) ~'f)
        ))))

(defmacro def-pred [ name args & body ] "that takes a name, an arg vector, and a body of one or more expressions. The macro defines two predicate functions: a regular one and its negated version. The name of the negated version is not-{name}"
    `(do
        (defn ~name ~args ~@body)
        (defn  ~(symbol  (str "not-" (str name)) ) ~args (not (do ~@body)) )))

(defmacro defn-curry [ name args & body ]
    )

(defn find-between "Returns a sequence of elements in l contained in (a, b)"
    [a b l]
    (->> l
         (drop-while #(not= a %))
         rest
         (take-while #(not= b %))))

(defmacro IF "Provides a conditional statement that is syntactically a bit more similar to those found in languages like Pascal or Fortran. It has the following form: (IF condition :THEN exp1 exp2 ... :ELSE exp3 exp4 ...)"
    [condition & exprs]
    `(if ~condition
       (do ~@(find-between :THEN :ELSE exprs) )
       (do ~@(find-between :ELSE :THEN exprs) )))