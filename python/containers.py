

array = [1, 2, 3, "hello"]
for i in array:
	print(i)

print("=======================================================")

dict1 = {'name': 'Ellie', 'type': 'Elephant'}

# name
# type
for i in dict1:
	print(i)

print("=======================================================")

# Python 3, iteritems is removed
for key, value in dict1.items():
	print(key, value)

print("=======================================================")

"""
name test
id 123
"""
for key, value in {'name': 'test', 'id': 123}.items():
	print(key, value)

print("=======================================================")
# array of dictionaries
animals = [{'name': 'Nemo', 'type': 'Ikan'},
			{'name': 'Ellie', 'type': 'Elephant'},
			{'name': 'ScoobyDoo', 'type': 'Dog'},
			{'name': 'Comel', 'type': 'Cat'}]

"""
name Nemo
type Ikan
name Ellie
type Elephant
name ScoobyDoo
type Dog
...
"""
for animal in animals:
	for key, value in animal.items():
		print(key, value)

print("=======================================================")

seasons = ["Spring", "Summer", "Fall", "Winter"]

# 0 Spring
# 1 Summer
# 2 Fall
# 3 Winter
for index, elem in enumerate(seasons):
	print(index, elem)
