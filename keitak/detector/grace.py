
# duplicate detector which is rotate and shift tolerant.
# the name Grace is taken from the character of SOUND VOLTEX III GRAVITY WARS, an arcade rhythm game from KONAMI.

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

    def scaladiv(self, other):
        return Point(self.x / other, self.y / other)

    def dot(self, other):
        return self.x * other.x + self.y * other.y

    def cross(self, other):
        return self.x * other.y - self.y * other.x

    def norm(self):
        return self.dot(self)

    def __hash__(self):
        return hash((self.x, self.y))

    def __str__(self):
        return "{},{}".format(self.x, self.y)

    def __repr__(self):
        return str(self)


class Matrix(object):
    def __init__(self, col1, col2):
        self.cols = [col1, col2]
        self.rows = [Point(col1.x, col2.x), Point(col1.y, col2.y)]

    def __mul__(self, other):
        return Matrix(self.vecmul(other.cols[0]), self.vecmul(other.cols[1]))

    def vecmul(self, vec):
        return Point(self.rows[0].dot(vec), self.rows[1].dot(vec))

    def __str__(self):
        return "[({}), ({})]".format(*self.cols)

    def __repr__(self):
        return str(self)


class ProblemInput(object):
    def __init__(self, file_path):
        with open(file_path) as input_file:
            # parse siruetto
            self.polygons = []
            n = int(input_file.readline())
            for i in range(n):
                n_vertex = int(input_file.readline())
                polygon = []
                for j in range(n_vertex):
                    point_str = input_file.readline()
                    polygon.append(Point(*map(Fraction, point_str.split(','))))
                self.polygons.append(polygon)
            # skeleton is ignored.

    def get_polygon_count(self):
        return len(self.polygons)


def check_equality(file_a, file_b):
    return get_transform(file_a, file_b) is None


def get_mapper(a_source, b_source, rotator):
    def mapper(point):
        return rotator.vecmul(point + a_source) + b_source
    return mapper


def get_transform(file_a, file_b):
    # returns: transformer FROM A TO B
    problem_a = ProblemInput(file_a)
    problem_b = ProblemInput(file_b)

    # we don't handle holes...
    if problem_a.get_polygon_count() > 1 or problem_b.get_polygon_count() > 1:
        return None

    a_source = problem_a.polygons[0][0]
    for b_source in problem_b.polygons[0]:
        for a_dest in problem_a.polygons[0][1:]:
            a_vector = a_dest - a_source
            for b_dest in problem_b.polygons[0]:
                if b_dest == b_source:
                    continue
                b_vector = b_dest - b_source
                if a_vector.norm() != b_vector.norm():
                    continue
                x = a_vector.dot(b_vector) / a_vector.norm()
                y = a_vector.cross(b_vector) / a_vector.norm()
                rotator = Matrix(Point(x, y), Point(-y, x))
                a_to_b = get_mapper(a_source, b_source, rotator)
                mapped_a_polygon = map(a_to_b, problem_a.polygons[0])
                if set(mapped_a_polygon) == set(problem_b.polygons[0]):
                    return a_source, b_source, rotator
    return None


def run_test():
    mat = Matrix(Point(0, 2), Point(3, 0))
    print(mat.vecmul(Point(1, 0)))
    print(set([Point(0, 2)]) == set([Point(0, 2)]))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/2.in"))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/3.in"))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/4.in"))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/5.in"))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/6.in"))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/7.in"))


if __name__ == '__main__':
    run_test()