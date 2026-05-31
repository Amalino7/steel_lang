class Square:
    def __init__(self, side):
        self.side = side
    def area(self):
        return self.side * self.side

class Circle:
    def __init__(self, radius):
        self.radius = radius
    def area(self):
        return 3.14159 * self.radius * self.radius

def get_area(s):
    return s.area()

sq = Square(10)
circ = Circle(5)

i = 0
total = 0
while i < 1000000:
    if i % 2 == 0:
        total += get_area(sq)
    else:
        total += get_area(circ)
    i += 1
print(total)
