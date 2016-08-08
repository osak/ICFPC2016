from fractions import Fraction


class Point(object):
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __neg__(self):
        return Point(-self.x, -self.y)

    def scaladiv(self, other):
        return Point(self.x / other, self.y / other)

    def dot(self, other):
        return self.x * other.x + self.y * other.y

    def cross(self, other):
        return self.x * other.y - self.y * other.x

    def norm(self):
        return self.dot(self)

    def reflect_on_x_axis(self):
        return Point(self.x, -self.y)

    def __hash__(self):
        return hash((self.x, self.y))

    def __str__(self):
        return "{},{}".format(self.x, self.y)

    def __repr__(self):
        return str(self)


polys = int(input())
for i in range(polys):
    n = int(input())
    for j in range(n):
        input()


skeletons = int(input())
left, right = Point(1000000, 0), Point(-10000000, 0)
all_points = []
for i in range(skeletons):
    point_strs = input()
    points = [Point(*map(Fraction, point_str.split(','))) for point_str in point_strs.split()]
    for point in points:
        if point.x < left.x:
            left = point
        if right.x < point.x:
            right = point
    all_points += points
print(left, right, (left - right).norm())

for point in all_points:
    if (point - left).norm() == 1:
        print(point)

norms = [(point - Point(0, 1)).norm() for point in all_points]
norms.sort()
for norm in norms:
    print(norm)
