from operator import itemgetter
import functools

array = [1, 2, 3, "hello"]
for i in array:
	print(i)

print("=======================================================")

dict1 = {'name': 'Ellie', 'type': 'Elephant'}

# name
# type
for i in dict1:
	print(i)

for value in dict1.values():
	print(value)

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

print("Python list comprehension -- remove Ellie from animals list")
animals = [anim for anim in animals if not anim['name'] == "Ellie"]
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


print("=======================================================")
print("Dynamically insert data structure")

farm = []

farm.append({'name': 'nemo', 'type': 'ikan'})
farm.append({'name': 'dumbo', 'type': 'elephant'})

for stuff in farm:
	for key, value in stuff.items():
		print("key={}, value={}".format(key, value))

print("num of key in farm_dict = %d" % len(farm))

print("=======================================================")
print("Array to string")

int_list = ["123", "44", "122", "1256", 45, 234]
int_list.append(1111111111)

print(str(int_list))


print("=======================================================")
print("Sort array of hashes by key")

table = [{'id': 1, 'entry': "entry1"},
		{'id': 4, 'entry': "entry4"},
		{'id': 3, 'entry': "entry3"},
		{'id': 2, 'entry': "entry2"}]

print("Before sort by id")
for cell in table:
	print("id={}, entry={}".format(cell['id'], cell['entry']))

print("After sort by id")
table = sorted(table, key=itemgetter('id'))

for cell in table:
	print("id={}, entry={}".format(cell['id'], cell['entry']))


print("Delete and rearrange id")
table.pop(1) # delete id=2

for cell in table:
	if cell['id'] > 1:
		cell['id'] -= 1

for cell in table:
	print("id={}, entry={}".format(cell['id'], cell['entry']))


print("=======================================================")
print("Sort array using custom comparator")

def mysorter(elem1, elem2):
	if elem1['id'] > elem2['id']:
		return 1
	elif elem1['id'] == elem2['id']:
		if elem1['sender_id'] >= elem2['sender_id']:
			return -1
		else:
			return 1
	else:
		return -1

# if 'id' are the same, then sort by 'sender_id'
table = [{'id': 1, 'sender_id': 1, 'entry': "entry1"},
		{'id': 4, 'sender_id': 2, 'entry': "entry4"},
		{'id': 3, 'sender_id': 1, 'entry': "entry3_1"},
		{'id': 3, 'sender_id': 2, 'entry': "entry3_2"},
		{'id': 3, 'sender_id': 3, 'entry': "entry3_3"},
		{'id': 2, 'sender_id': 4, 'entry': "entry2"}]

print("Before sort")
for entry in table:
	print(str(entry))


print("After sort")

table = sorted(table, key=functools.cmp_to_key(mysorter))
for entry in table:
	print(str(entry))

print("Testing list comprehension")
table = [entry for entry in table if not (entry['id'] == 3 and entry['sender_id'] == 1)]

for entry in table:
	print(str(entry))

if 0 > 3:
	print("WTH 0 > 3??")
else:
	print("Cool, that works")
