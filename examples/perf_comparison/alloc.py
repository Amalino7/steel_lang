class Point:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

i = 0
while i < 1000000:
    p = Point(i, i + 1, i + 2)
    i += 1
print(i)
