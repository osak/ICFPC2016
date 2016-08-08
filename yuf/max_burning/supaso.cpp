#include <cmath>
#include <complex>
#include <set>
#include "rational.cpp"

using namespace std;

#define ALL(c) (c).begin(), (c).end()

typedef int Weight;

struct Edge {
    int src, dst;
    Weight weight;
    Edge(int src, int dst, Weight weight) :
            src(src), dst(dst), weight(weight) { }
};

typedef vector<Edge> Edges;
typedef vector<Edges> Graph;

Graph segmentArrangement(const vector<Line> &ss, vector<Point> &ps) {
    for (int i = 0; i < ss.size(); ++i) { // O(n^2)
        ps.push_back( ss[i][0] );
        ps.push_back( ss[i][1] );
        for (int j = i+1; j < ss.size(); ++j)
            if (intersectSS(ss[i], ss[j]))
                ps.push_back( crosspoint(ss[i], ss[j]) );
    }

    sort(ALL(ps));
    auto ptr = unique(ALL(ps));
    ps.erase(ptr, ps.end());

    Graph g(ps.size());
    for (int i = 0; i < ss.size(); ++i) {
        vector< pair<Rational, int> > list;
        for (int j = 0; j < ps.size(); ++j)
            if (intersectSP(ss[i], ps[j]))
                list.push_back(make_pair(norm(ss[i][0]-ps[j]), j));
        sort(ALL(list));
        for (int j = 0; j+1 < list.size(); ++j) {
            int a = list[j].second, b = list[j+1].second;
            g[a].push_back( Edge(a, b, -5) );
            g[b].push_back( Edge(b, a, -5) );
        }
    }
    return g;
}

#define curr_s(P, i) P[i]
#define next_s(P, i) P[(i+1)%P.size()]
enum { OUT_s, ON_s, IN_s };
int contains(const Polygon& poly, const Point& p) {
    bool in = false;
    for (int i = 0; i < poly.size(); ++i) {
        Point a = curr_s(poly,i) - p, b = next_s(poly,i) - p;
        if (a.y > b.y) swap(a, b);
        if (a.y <= Rational(0) && Rational(0) < b.y)
        if (cross(a, b) < 0) in = !in;
        if (cross(a, b) == 0 && dot(a, b) <= 0) return ON_s;
    }
    return in ? IN_s : OUT_s;
}

vector<Point> convex_hull(vector<Point> ps) {
    int n = ps.size(), k = 0;
    sort(ps.begin(), ps.end());
    vector<Point> ch(2*n);
    for (int i = 0; i < n; ch[k++] = ps[i++]) // lower-hull
        while (k >= 2 && ccw(ch[k-2], ch[k-1], ps[i]) <= 0) --k;
    for (int i = n-2, t = k+1; i >= 0; ch[k++] = ps[i--]) // upper-hull
        while (k >= t && ccw(ch[k-2], ch[k-1], ps[i]) <= 0) --k;
    ch.resize(k-1);
    return ch;
}

Rational area(const vector<Point>& P) {
    Rational A = 0;
    for (int i = 0; i < P.size(); ++i)
        A += cross(curr_s(P, i), next_s(P, i));
    return A / Rational(2);
}

bool PPintersect(Polygon& p, Polygon& q) {
    set<Rational> event_set;
    for (auto pp : p) {
        event_set.insert(pp.x);
    }
    for (auto qp : q) {
        event_set.insert(qp.x);
    }
    vector<Rational> events;
    for (auto event : event_set) {
        events.push_back(event);
    }

    for (int i = 0; i < events.size() - 1; ++i) {
        const Rational x = (events[i] + events[i + 1]) / 2;
        Line scanner(Point(x, -1e9), Point(x, 1e9));
        vector<pair<Rational, int>> crossing;

        for (int i = 0; i < p.size(); ++i) {
            Line edge = p.get_edge(i);
            if (intersectSS(scanner, edge)) {
                crossing.emplace_back(crosspoint(scanner, edge).y, 0);
            }
        }
        for (int i = 0; i < q.size(); ++i) {
            Line edge = q.get_edge(i);
            if (intersectSS(scanner, edge)) {
                crossing.emplace_back(crosspoint(scanner, edge).y, 1);
            }
        }
        sort(ALL(crossing));
        bool states[2] = {false, false};
        for (int i = 0; i < crossing.size(); ++i) {
            auto event = crossing[i];
            states[event.second] = !states[event.second];
            if (i + 1 < crossing.size() && event.first == crossing[i + 1].first) {
                continue;
            }
            if (states[0] && states[1]) {
                return true;
            }
        }

    }
    return false;
}