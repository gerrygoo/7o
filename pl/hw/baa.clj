(import 'clojure.lang.IFn)

(defn third [lst] (nth lst 2))
(defn fourth [lst] (nth lst 3))

(deftype Closure [env params body] Ifn 
	(invoke [self args]
		($eval body (merge @env (zipmap params args)) )
	)
	(applyTo [self args] (self args))
)

(def globals {
	'EQ =
	'CAR first
	'CDR rest
	'CONS cons
	'ATOM #(or (= % ()) (not (list? %)) )
	'* *
	'+ +
	})

(defn $eval [expr env]
	(cond
		; 1 Variable references
		(symbol? expr) 
			(if (contains? env expr)
					(get env expr)
					(throw (RuntimeException. (str "Unbound symbol: " expr) ) )
			)

		(list? expr)

			; 2 Special forms
			(case (first expr)
				; empty list
				nil () 

				; quote
				'quote (second expr)

				; if
				(if  (eval (second exp) env)
					(eval (third exp) env)
					(eval (fourth expr) env)
				)

				label
				(let [closure ($eval (third expr) env) ]
					(swap! (.env closure)
						#(assoc % (second expr) closure)
					)
					closure
				)

				; lambda
				lambda
				(->Closure (atom env) (second expr) (third expr))

				; list but not those, function call
				(apply 
					(eval (first expr) env)
					(map #(eval % env) (rest expr)) 
				)
			)

		; 3 everything else evals to itself
		:else expr
	)
)