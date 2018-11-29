(println
	(take 5 (iterate (partial reductions +') (repeat 1)))
)