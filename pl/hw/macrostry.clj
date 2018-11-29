(defmacro do-loop [& body] "the expressions in the body are evaluated sequentially, and then the condition is evaluated. If the final form uses a :while keyword, the body of the loop is repeated while the condition holds true. On the other hand, if the final form uses an :until keyword, the body of the loop is repeated while the condition holds false (or in other words, the loop terminates when the condition is true). Fails to return nil."
    `((defn ~'f [ ]
        (cond
            (or
                (and (= ~(first (last body)) :while) ~(last (last body)) )
                (and (= ~(first (last body)) :until) (not ~(last (last body))) )
            )
            (do ~@(butlast body) ~'f)
        ))))

; (defn  ~'(symbol  (str "not-" (str name)) ) ~args (not (~@body)) )