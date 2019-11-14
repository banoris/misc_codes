

# -------------------------------------------- #
# https://www.youtube.com/watch?v=MYAEv3JoenI
def check(func):
	def inner_func(a, b):
		if b == 0:
			print("Can't divide by 0")
			return
		return func(a, b);
	return inner_func

def div(a, b):
	return a / b

div = check(div)

print("===== Without using @ decorator =====")
print('10/5 = {}'.format(int(10 / 5)))
print("div(10, 5) = %d" % div(10, 5))
print(div(10, 0))

# After using decorator...
@check
def div_deco(a, b):
	return a / b

print("===== After using @ decorator =====")
print('10/5 = {}'.format(int(10 / 5)))
print("div_deco(10, 5) = %d" % div_deco(10, 5))
print('div_deco(10, 0) = {}'.format(div_deco(10, 0)))



# -------------------------------------------- #
# https://stackoverflow.com/questions/5929107/decorators-with-parameters
# https://www.artima.com/weblogs/viewpost.jsp?thread=240845#decorator-functions-with-decorator-arguments

print("===== Decorator with args")

def decorator_with_args(arg1, arg2, arg3):
	print("Inside decorator_with_args")
	def wrap_func(func):
		print("Inside wrap_func()")
		def wrap_wrap_func(*args, **kwargs):
			print("Inside wrap_wrap_func")
			print('Decorator arg1={}, arg2={}, arg3={}'.format(arg1, arg2, arg3))

			# TODO: not working? prints None
			print(kwargs.get('a1'))

			return func(*args, **kwargs)
		return wrap_wrap_func
	return wrap_func

@decorator_with_args("hello", "world", 111)
def hello_there(a1, a2):
    print('Hello a1={}, a2={}'.format(a1, a2))

hello_there(11, 22)
# -------------------------------------------- #
