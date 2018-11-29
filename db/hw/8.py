from functools import reduce
from itertools import product, combinations

def printit(a): print([i for i in a])

def subsets(s):
    sets = []
    for i in range(1 << len(s)):
        subset = frozenset( [s[bit] for bit in range(len(s)) if is_bit_set(i, bit)] )
        sets.append(subset)
    return sets

def is_bit_set(num, bit):
    return num & (1 << bit) > 0


dataset = [
	frozenset( { "milk", "bread", "eggs" } ) ,
	frozenset( { "milk", "juice" } ) ,
	frozenset( { "juice", "butter" } ) ,
	frozenset( { "milk", "bread", "eggs" } ) ,
	frozenset( { "coffee", "eggs" } ) ,
	frozenset( { "coffee" } ) ,
	frozenset( { "coffee", "juice" } ) ,
	frozenset( { "milk", "bread", "cookies", "eggs" } ) ,
	frozenset( { "cookies", "butter" } ) ,
	frozenset( { "milk", "bread" } )
]
MIN_SUPP = 0.2
MIN_CONFIDENCE = 0.9

items = set()
for i in dataset: items |= i

items = frozenset(items)
items_l = list(items)


def freq(s, dataset):
	return len( [ None for i in dataset if s <= i ] )

def supp(subset, dataset):
	return freq(subset) / len(dataset)

def supported(subset, dataset, support_threshold):
	return supp(subset, dataset) >= support_threshold

def conf(x, y):
	return supp(x | y) / supp(x)



# A-PRIORI

def join(set_a, set_b):
	return { i | j for i in set_a for j in set_b if i != j and i & j }

def selfjoin(some_set):
	# don't take { a, a }
	# return map( frozenset, filter(lambda x: len(x) > 1, map( lambda s: reduce(lambda c, n: c | n, s), product(some_set, {frozenset(i) for i in items} ) )) )

	return map( frozenset, filter(lambda x: len(x) > 1, map( lambda s: reduce(lambda c, n: c | n, s), product(some_set, repeat = 2 ) )) )
	return join(some_set, some_set)

def prune(new, old):
	return frozenset(
		filter(
			lambda new_set: all( subset in old for subset in map(frozenset, combinations(new_set, len(new_set) - 1 )) ),
		new)
	)

def apriori(items, support_threshold):
	ap_supported = set()

	c = [ frozenset([i]) for i in items ]
	l = frozenset(filter(supported, c))

	while c:
		for el in l: ap_supported.add(el)
		c_prev, l_prev = c, l
		c, l = prune(selfjoin(l_prev), l_prev), frozenset(filter(supported, c))

	return frozenset(ap_supported)



# BRUTE_FORCE
# all_subsets = frozenset(subsets(items_l)[1:])
# bf_supported = frozenset(filter(supported, all_subsets))

supported_subsets = ap_supported
all_pairs = product(supported_subsets, repeat = 2)
disjoint_pairs = filter(lambda pair: len(pair[0] & pair[1]) == 0 ,all_pairs)
answer_pairs = filter(lambda a: conf(*a) >= MIN_CONFIDENCE , disjoint_pairs)


# RELATIONS
for relation in answer_pairs:
	print(f'{set(relation[0])} => {set(relation[1])}')


a = frozenset(
		{frozenset({1, 2}),
		frozenset({1, 3}),
		frozenset({1, 4}),
		frozenset({1, 5}),
		frozenset({2, 3}),
		frozenset({2, 4}),
		frozenset({2, 5}),
		frozenset({3, 4}),
		frozenset({3, 5}),
		frozenset({4, 5})}
	)
