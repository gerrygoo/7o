
EMPTY_LIST = "()"
QUOTE = "'"


third = lambda lst: lst[2]
fourth = lambda lst: lst[3]
 
# TODO
first =
symbol? = lambda epr: True
lst? = lambda epr: True
lempty? = lambda lst: lst == "[]"


def eval(expr, env):
	
	# 1 Variable refs
	if symbol?(expr):

		if expr in env:
			return env[expr]
		else:
			throw f"Unbound symbol{expr}"

	# 2 Special forms
	elif lst?(expr):
		if lempty?(expr):
			return EMPTY_LIST
		else:
			return expr[1:]

	else:
		pass