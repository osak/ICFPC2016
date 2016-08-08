from fractions import Fraction


class Point(object):
    def __init__(self, x, y):
        self.x = Fraction(x)
        self.y = Fraction(y)

    def __sub__(self, other):
        return Point(self.x - other.x, self.y - other.y)

    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def __eq__(self, other):
        return isinstance(other, Point) and self.x == other.x and self.y == other.y

    def __neg__(self):
        return Point(-self.x, -self.y)

    def scala_mul(self, scala):
        return Point(self.x * scala, self.y * scala)

    def dot(self, other):
        return self.x * other.x + self.y * other.y

    def cross(self, other):
        return self.x * other.y - self.y * other.x

    def norm(self):
        return self.dot(self)

    def ortho(self):
        return Point(-self.y, self.x)

    def proj(self, p1, p2):
        t = (self - p1).dot(p1 - p2) / (p1 - p2).norm()
        return p1 + (p1 - p2).scala_mul(t)

    def reflect(self, p1, p2):
        return self + (self.proj(p1, p2) - self).scala_mul(Fraction(2))

    def __hash__(self):
        return hash((self.x, self.y))

    def __str__(self):
        return "{},{}".format(self.x, self.y)

    def __repr__(self):
        return str(self)


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


def main():
    polys = int(input())
    for i in range(polys):
        n = int(input())
        for j in range(n):
            input()

    skeletons = int(input())
    focus, far_point = Point(1000000, 0), Point(-10000000, 0)

    point_to_index = dict()
    all_points = []
    edges = []
    for i in range(skeletons):
        point_strs = input()
        points = [Point(*map(Fraction, point_str.split(','))) for point_str in point_strs.split()]
        for point in points:
            if point.x < focus.x:
                focus = point
            if far_point.x < point.x:
                far_point = point
            if point not in point_to_index:
                point_to_index[point] = len(all_points)
                all_points.append(point)
        edges.append((point_to_index[points[0]], point_to_index[points[1]]))

    graph = [[] for _ in range(len(all_points))]
    for fr, to in edges:
        graph[fr].append(to)
        graph[to].append(fr)

    source_to_dest = {
        Point(0, 1): focus,
        Point(1, 0): far_point
    }
    direction = [Point(-1, 0), Point(0, 1)]
    facets = []

    def nexts(src, prv):
        next_candidate = []
        for adj_index in graph[point_to_index[src]]:
            next_cand = all_points[adj_index]
            next_candidate.append(next_cand)
        if prv:
            next_direction = prv.reflect(src, src + (src - focus).ortho()) - src
        else:
            for next_cand in next_candidate:
                if next_cand != focus:
                    next_direction = next_cand - src
        for point in all_points:
            if (point - focus).norm() < (src - focus).norm() and (point - src).cross(next_direction) == 0 and (point - src).dot(next_direction) > 0:
                next_candidate.append(point)
        next_candidate = sorted(list(set(next_candidate)), key=lambda p: p.norm())
        def is_good_adj(point):
            return (point - focus).norm() < (src - focus).norm() and point not in [prv, focus]
        next_candidate = list(filter(is_good_adj, next_candidate))
        goal_list = [point for point in next_candidate if (point - focus).norm() == 1]
        if goal_list:
            return goal_list
        else:
            return [point for point in next_candidate if nexts(point, src)]

    for path_id in range(2):
        pos, prev = far_point, None
        accum = 0
        while (focus - pos).norm() > 1:
            next_candidates = nexts(pos, prev)
            next_pos = next_candidates[path_id % len(next_candidates)]

            next_accum = accum + sqrt_frac((next_pos - pos).norm())

            facets_source = [Point(0, 1), Point(1, 0) + direction[path_id].scala_mul(accum), Point(1, 0) + direction[path_id].scala_mul(next_accum)]
            facets_dest = [focus, pos, next_pos]
            for src, dst in zip(facets_source, facets_dest):
                source_to_dest[src] = dst
            facets.append(facets_source)

            pos, prev = next_pos, pos
            accum = next_accum

    sources = list(source_to_dest.keys())
    source_to_index = dict((v, i) for i, v in enumerate(sources))

    print(len(sources))
    for point in sources:
        print(point)
    print(len(facets))
    for facet in facets:
        encoded = [len(facet)] + [source_to_index[point] for point in facet]
        print(' '.join(map(str, encoded)))
    for point in sources:
        print(source_to_dest[point])


def do_test():
    p = Point(3,4)
    assert(p.dot(p.ortho()) == 0)
    assert(p.cross(p.ortho()) == p.norm())
    q = Point(5, 6)
    o = Point(0, 0)
    r = q.reflect(o, p)
    assert((o - p).dot(q - r) == 0)
    m = (q + r).scala_mul(Fraction(1, 2))
    assert((o - m).cross(p - m) == 0)


if __name__ == '__main__':
    do_test()
    main()