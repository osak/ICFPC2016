#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <assert.h>
#include <random>
#include "supaso.cpp"

using namespace std;

struct GeometricGraph {
    Graph graph;
    vector<Point> points;
    GeometricGraph(const Graph _graph, const vector<Point> _points): graph(_graph), points(_points) {
    }
};

struct Problem {
    vector<Line> segments;
    vector<Point> points;

    Problem(const vector<Line> &segments) : segments(segments), points() { }

    GeometricGraph arrange() {
        points.clear();
        Graph g = segmentArrangement(segments, points);
        return GeometricGraph(g, points);
    }
};

bool pointEq (const Point &p1, const Point &p2) {
    return p1.x == p2.x && p1.y == p2.y;
};

Point get_point(const string s) {
    for (int j = 0; j < s.size(); j++) {
        if (s[j] == ',') {
            return Point(get_rational(s.substr(0, j)), get_rational(s.substr(j + 1)));
        }
    }
    assert(false);
}

struct Segment {
    Line line;
    bool visible;
    Segment (Line line, bool visible) : line(line), visible(visible) {}
};

void printLines(vector<Line> vec, vector<string> history) {
    int n = vec.size();
    cout << n << endl;
    for (int i = 0; i < vec.size(); ++i) {
        cout << vec[i].p1 << ' ' << vec[i].p2 << ' ' << vec[i].visible << endl;
    }
    cout << history.size() << endl;
    for (auto s: history) {
        cout << s << endl;
    }
}

void printProblem() {
}

bool segEq (const Line &l1, const Line &l2) {
    return pointEq(l1.p1, l2.p1) && pointEq(l1.p2, l2.p2)
    || pointEq(l1.p1, l2.p2) && pointEq(l1.p2, l2.p1);
};

// O(n)
void addIfUnique(vector<Line> &lines, const Line &l) {
    for (auto line : lines) {
        if(segEq(line, l)) return;
    }
    lines.push_back(l);
    return;
}

void init() {
    int siruettonokazu;
    cin >> siruettonokazu;
    for (int i = 0; i < siruettonokazu; ++i) {
        // ignore
        int n;
        cin >> n;
        for (int j = 0; j < n; ++j) {
            string s;
            cin >> s;
        }
    }

    int segment_count;
    cin >> segment_count;

    vector<Line> segs;
    for (int i = 0; i < segment_count; ++i) {
        string x, y;
        cin >> x >> y;
        segs.push_back(Line(get_point(x), get_point(y)));
    }

    auto geograph = Problem(segs).arrange();

    vector<Line> lines;
    for (auto edges : geograph.graph) {
        for (auto edge : edges) {
            int a = edge.src;
            int b = edge.dst;
            addIfUnique(lines, Line(geograph.points[a], geograph.points[b]));
        }
    }

    printLines(lines, {});
}

Line lineInput(bool b = false) {
    string s, t;
    cin >> s >> t;
    if (!b) return Line(get_point(s), get_point(t));
    int visible;
    cin >> visible;
    return Line(get_point(s), get_point(t), visible);
}


inline Line mirror(const Line& l, const Line& target) {
    return Line(reflection(l, target.p1), reflection(l, target.p2));
}

void unfold() {
    /* webapp kara no nyuuryoku */
    int sukerutonnokazu = 0;
    cin >> sukerutonnokazu;
    vector<Line> skeleton;
    for (int i = 0; i < sukerutonnokazu; ++i) {
        skeleton.push_back(lineInput(true));
    }

    int unfoldsuruyatu = 0;
    cin >> unfoldsuruyatu;
    Line jiku = lineInput();
    vector<Line> mawasuyatu;
    for (int i = 1; i < unfoldsuruyatu; ++i) {
        auto l = lineInput();
        mawasuyatu.push_back(l);
    }

    int historySize;
    cin >> historySize;
    cin.ignore();
    vector<string> history;
    for (int i = 0; i < historySize; ++i) {
        string s;
        getline(cin, s);
        history.push_back(s);
    }
    /* webapp kara no nyuuryoku owari */


    int x = 0;
    for (int i = 0; i < mawasuyatu.size(); ++i) {
        auto l = mawasuyatu[i];
        addIfUnique(skeleton, mirror(jiku, l));

        if (jiku.p1 == l.p1
        || jiku.p2 == l.p1) {
            x = ccw(jiku.p1, jiku.p2, l.p2); 
        } else if (jiku.p1 == l.p2
                || jiku.p2 == l.p2 ) {
            x = ccw(jiku.p1, jiku.p2, l.p1); 
        }
    }

    if (x == RIGHT) swap(jiku.p1, jiku.p2);
    stringstream ss;
    ss << "fold " << jiku.p1 << ' ' << jiku.p2;
    history.push_back(ss.str());
    printLines(skeleton, history);
}

bool outputProblem (const vector<Line> &skeleton) {
#ifdef _DEBUG
    cout << "checking new skeleton validity:" << endl;
#endif
    vector<Point> points;
    for (auto line : skeleton) {
        if (!line.visible) continue;
        points.push_back(line.p1);
        points.push_back(line.p2);
    }
    sort(points.begin(), points.end());
    points.erase(unique(points.begin(), points.end()), points.end());

    auto v = convex_hull(points);

    int n = v.size();
    for (int i = 0; i < v.size(); ++i) {
        bool ok = false;
        for (auto line : skeleton) {
            auto tmp = Line(v[i], v[(i + 1)%n]);
            ok |= segEq(line, Line(v[i], v[(i + 1)%n])) || segEq(line, Line(v[(i + 1)%n], v[i]));
            if (ok) break;
        }
        if (!ok) return false;
    }

    cout << 1 << endl << n << endl;
    for (int i = 0; i < n; ++i) {
        cout << v[i].x << ',' << v[i].y << endl;
    }
    int m = skeleton.size();
    cout << m << endl;
    for (int i = 0; i < m; ++i) {
        if (!skeleton[i].visible) continue;
        auto &p1 = skeleton[i].p1;
        auto &p2 = skeleton[i].p2;
        cout << p1.x << ',' << p1.y << ' ' << p2.x << ',' << p2.y << endl;
    }
    return true;
}

void end() {
    /* webapp kara no nyuuryoku */
    int sukerutonnokazu = 0;
    cin >> sukerutonnokazu;
    vector<Line> skeleton;
    for (int i = 0; i < sukerutonnokazu; ++i) {
        skeleton.push_back(lineInput(true));
    }

//    int unfoldsuruyatu = 0;
//    cin >> unfoldsuruyatu;
//    Line jiku = lineInput();
//    vector<Line> mawasuyatu = lineInput();
//    for (int i = 1; i < unfoldsuruyatu; ++i) {
//        auto l = lineInput();
//        mawasuyatu.push_back(l);
//    }

    vector<string> history;
    int historySize;
    cin >> historySize;
    cin.ignore();
    for (int i = 0; i < historySize; ++i) {
        string s;
        getline(cin, s);
        history.push_back(s);
    }
    /* webapp kara no nyuuryoku owari */

    if (outputProblem(skeleton)) {
        for (auto s : history) {
            cout << s << endl;
        }
    } else {
        cout << "凸じゃないんでシルエットわかんないです＾＾；" << endl;
    }
}

enum ORDER {
    INIT, UNFOLD, END
};

int main() {
    string order;
    cin >> order;

    int o = 0;
    if (order == "init") o = INIT;
    else if (order == "unfold") o = UNFOLD;
    else if (order == "end") o = END;

    switch (o) {
        case INIT:
            init();
            break;
        case UNFOLD:
            unfold();
            break;
        case END:
            end();
            break;
    }

    return 0;
}
