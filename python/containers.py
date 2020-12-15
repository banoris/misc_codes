from operator import itemgetter
import functools
from collections import namedtuple, OrderedDict

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


print("=======================================================")
print("Tuple as key")

Event = namedtuple("Event", ["node", "clock"])

ev1 = Event(node=1, clock=10)
ev11 = Event(node=1, clock=20)
ev12 = Event(node=1, clock=3)
ev2 = Event(node=2, clock=1)
ev3 = Event(node=3, clock=1)
ev5 = Event(node=1, clock=1)


board = {ev1: "hello1_1",
         ev2: "hello2_1",
         ev3: "hello3_1",
         # ev5: "hello1_5", # weird bug when sorted?
         ev11: "hello1_2",
         ev12: "hello1_3"} # override ev1, since key(ev1) == key(ev5)
# board = sorted(board, key=itemgetter('node', 'clock')) # ERROR?
# board2 = sorted(board.items(), key=lambda x:(x[0].clock, x[0].node)) # OK!
# board = sorted(board, key=lambda x:(x.node, x.clock)) # now board is list?
# board = sorted(board.items(), key=itemgetter('node', 'clock'))

print("=== Default board, no sorting ===")
print(board)

# NOTE: sorted() returns a list!! dict aka map is unordered by default
for key, val in board.items():
    print(f"key.node={key.node}, key.clock={key.clock}, value={val}")

# by default, sort by first key, i.e. 'node'
ord_board = OrderedDict(sorted(board.items()))
print("=== Default sorted, by first key 'node' ===")
for key, val in ord_board.items():
    print(f"key.node={key.node}, key.clock={key.clock}, value={val}")

# NOTE: you misunderstood! x[1] is the value of the dict, so you are sorting
# based on the strings: ["hello1_3", "hello2_1", ...]
print("=== sort by 'clock' field ===")
# FIXME: ord_board = OrderedDict(sorted(board.items(), key=lambda x: x[1]))
ord_board = OrderedDict(sorted(board.items(), key=lambda x: x[0].clock))

for key, val in ord_board.items():
    print(f"key.node={key.node}, key.clock={key.clock}, value={val}")

print("=== sort by 'node' and then 'clock' ===")
# E.g., (node=1, clock=30) < (node2, clock=2).
# If you print, (node=1, clock=30) will appear first
ord_board = OrderedDict(sorted(board.items(), key=lambda x: (x[0].clock, x[0].node)))

"""
NOTE: as you can see, (node=2, clock=1) is printed first compared to
      (node=3, clock=1). Eventhough both have same clock values,
      node2 printed first since node2 < node3
=== sort by 'node' and then 'clock' ===
key.node=2, key.clock=1, value=hello2_1
key.node=3, key.clock=1, value=hello3_1
key.node=1, key.clock=3, value=hello1_3
"""
for key, val in ord_board.items():
    print(f"key.node={key.node}, key.clock={key.clock}, value={val}")

