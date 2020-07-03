import random

# https://www.tutorialspoint.com/What-is-difference-between-self-and-init-methods-in-python-Class
class Rectangle:
   def __init__(self, length, breadth, unit_cost=0):
       print("__init__ Rectangle")
       self.length = length
       self.breadth = breadth
       self.unit_cost = unit_cost
   def get_area(self):
       return self.length * self.breadth
   def calculate_cost(self):
       area = self.get_area()
       return area * self.unit_cost

class Square:
   def __init__(self, edge, unit_cost=0):
       print("__init__ Square")
       self.length = edge
       self.breadth = edge
       self.unit_cost = unit_cost
   def get_area(self):
       return self.length * self.breadth
   def calculate_cost(self):
       area = self.get_area()
       return area * self.unit_cost


# breadth = 120 units, length = 160 units, 1 sq unit cost = Rs 2000
r = Rectangle(160, 120, 2000)
print("Area of Rectangle: %s sq units" % (r.get_area()))

square = Square(100, 2000)
print("Area of Square: %s sq units" % (square.get_area()))

# Will this instantiate the Rectangle or Square object?
# I.e., will you see the __init__ log printed out? NOPE!
# No instantiation done here - we are only creating a reference
# to both Rectangle and Square classes
shapes = [Rectangle, Square]
print(shapes)
# ==============================

# dynamic object instantiation in runtime
print("================================")
some_cond = random.choice((True, False))
if (some_cond):
    my_shape = shapes[0](10, 20, 3333)
else:
    my_shape = shapes[1](10, 2222)
print("my_shape Area=", my_shape.get_area())
# ==============================
