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


def area(polygon):
    ans = Fraction(0)
    for i in range(len(polygon)):
        ans += polygon[i].cross(polygon[(i + 1) % len(polygon)]) / Fraction(2)
    return ans


class ProblemInput(object):
    def __init__(self, input_file=None, polygons=None):
        # parse siruetto
        self.polygons = []
        if polygons is not None:
            self.polygons = polygons
            return
        elif input_file is not None:
            n = int(input_file.readline())
            for i in range(n):
                n_vertex = int(input_file.readline())
                polygon = []
                for j in range(n_vertex):
                    point_str = input_file.readline()
                    polygon.append(Point(*map(Fraction, point_str.split(','))))
                self.polygons.append(polygon)
        # skeleton is ignored.
        else:
            raise Exception("no info to construct ProblemInput")
        self.polygons.sort(key=area, reverse=True)

    def get_polygon_count(self):
        return len(self.polygons)


def check_equality(file_a, file_b):
    return get_transform(file_a, file_b) is None


def get_mapper(do_reflect, a_source, b_source, rotator):
    def mapper(point):
        if do_reflect:
            point = point.reflect_on_x_axis()
        return rotator.vecmul(point - a_source) + b_source
    return mapper


def get_transform(file_a, file_b):
    # returns: transformer FROM A TO B
    with open(file_a) as problem_file_a, open(file_b) as problem_file_b:
        problem_a = ProblemInput(problem_file_a)
        problem_b = ProblemInput(problem_file_b)
    return get_transform_body(problem_a, problem_b)


def basic_examine(problem_a, problem_b):
    if problem_a.get_polygon_count() != problem_b.get_polygon_count():
        return False

    polygon_count = problem_a.get_polygon_count()

    for i in range(polygon_count):
        if len(problem_a.polygons[i]) != len(problem_b.polygons[i]):
            return False
    return True


def get_transform_body(problem_a, problem_b):
    if not basic_examine(problem_a, problem_b):
        return None
    polygon_count = problem_a.get_polygon_count()

    for do_reflect in [False, True]:
        if do_reflect:
            a_source = problem_a.polygons[0][0].reflect_on_x_axis()
            a_dest = problem_a.polygons[0][-1].reflect_on_x_axis()
        else:
            a_source = problem_a.polygons[0][0]
            a_dest = problem_a.polygons[0][1]
        for i in range(len(problem_b.polygons[0])):
            b_source = problem_b.polygons[0][i]
            b_dest = problem_b.polygons[0][(i + 1) % len(problem_b.polygons[0])]
            a_vector = a_dest - a_source
            b_vector = b_dest - b_source
            if a_vector.norm() != b_vector.norm():
                continue
            x = a_vector.dot(b_vector) / a_vector.norm()
            y = a_vector.cross(b_vector) / a_vector.norm()
            rotator = Matrix(Point(x, y), Point(-y, x))
            a_to_b = get_mapper(do_reflect, a_source, b_source, rotator)

            ok = True
            for i in range(polygon_count):
                mapped_a_polygon = map(a_to_b, problem_a.polygons[i])
                # print(i)
                # print(list(mapped_a_polygon))
                # print(problem_b.polygons[0])
                if set(mapped_a_polygon) != set(problem_b.polygons[i]):
                    ok = False
            if ok:
                return do_reflect, a_source, b_source, rotator
    return None


def isqrt(n):
    x = n
    y = (x + 1) // 2
    while y < x:
        x = y
        y = (x + n // x) // 2
    return x


def square_or_none(x):
    candsq = isqrt(x)
    for sqx in range(max(candsq-2, 0), candsq+3):
        if sqx * sqx == x:
            return sqx
    return None


def sqrt_frac(frac):
    new_num = square_or_none(frac.numerator)
    new_denom = square_or_none(frac.denominator)
    if new_num is not None and new_denom is not None:
        return Fraction(new_num, new_denom)
    else:
        return None


def get_transform_with_scaling(problem_a, problem_b):
    if not basic_examine(problem_a, problem_b):
        return None
    area_ratio = area(problem_b.polygons[0]) / area(problem_a.polygons[0])
    sqrt_or_none = sqrt_frac(area_ratio)

    if sqrt_or_none is None:
        return None

    scaling_ratio = sqrt_or_none
    new_polygon = [Point(point.x * scaling_ratio, point.y * scaling_ratio) for point in problem_a.polygons[0]]
    new_problem_a = ProblemInput(polygons=[new_polygon])
    transform_or_none = get_transform_body(new_problem_a, problem_b)

    if transform_or_none is None:
        return None

    return (scaling_ratio,) + transform_or_none


def run_test():
    print(set([Point(0, 2)]) == set([Point(0, 2)]))
    print(area([Point(0, 0), Point(1, 0), Point(0, 1)]) == Fraction(1, 2))
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/2.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/3.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/4.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/5.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/6.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/166.in", "/home/icfpc/shared/problems/167.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/162.in", "/home/icfpc/shared/problems/168.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/3807.in", "/home/icfpc/shared/problems/98.in") is not None)
    print(get_transform("/home/icfpc/shared/problems/1.in", "/home/icfpc/shared/problems/8.in") is None)
    with open("/home/icfpc/shared/problems/1.in") as orig_file, open("/home/icfpc/shared/problems/8.in") as dest_file:
        print(get_transform_with_scaling(ProblemInput(orig_file), ProblemInput(dest_file)) is not None)
    print(get_transform("/home/icfpc/shared/problems/1270.in", "/home/icfpc/shared/problems/1275.in") is not None)


if __name__ == '__main__':
    run_test()