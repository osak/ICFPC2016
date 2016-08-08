#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <assert.h>
#include <random>
#include "supaso.cpp"
#include "amylase_comp.cpp"
//#include "rational.cpp"


using namespace std;

const int FACET_THRESHOLD = 8; // todo: speedup to remove this param.
vector<vector<int>> packing(vector<Rational> size, Rational bag_size) {
    vector<vector<int>> ans;
    if (size.size() == 0) {
        if (abs(bag_size) < EPS) {
            ans.push_back(vector<int>());
        }
        return ans;
    }

    vector<Rational> newsize(size.begin() , size.end() - 1);
    Rational new_bag_size = bag_size;
    for (int i = 0; i < FACET_THRESHOLD && new_bag_size > -EPS; ++i, new_bag_size -= size[size.size() - 1]) {
        vector<vector<int>> recur_ans = i ? packing(newsize, new_bag_size) : vector<vector<int>>();
        for (auto one_ans : recur_ans) {
            one_ans.push_back(i);
            ans.push_back(one_ans);
        }
    }

    return ans;
}

struct EnumerateSpanningTree {
    struct edge {
        int from, to, cost;

        bool operator<(const edge &q) const {
            return
                    (cost < q.cost) ||
                    (cost == q.cost && to < q.to) ||
                    (cost == q.cost && to == q.to && from < q.from);
        }
    };

    const int v;
    vector<edge> g;

    EnumerateSpanningTree (int _v) : v(_v), g(0) { }

    void add_edge(int from, int to) {
        g.push_back((edge) {from, to});
    }

    vector<vector<edge>> get_trees() {
        vector<vector<edge>> ans;
        // todo: speed up.
        // [XXX] restrict size.
        for (long long mask = 0; mask < (1LL << min((int)g.size(), 25)); ++mask) {
            if (__builtin_popcountll(mask) != v - 1) {
                continue;
            }
            UnionFind uf(v);
            bool ok = true;
            vector<edge> tree;
            for (int i = 0; i < g.size(); ++i) {
                if (mask & (1LL << i)) {
                    if (uf.same(g[i].from, g[i].to)) {
                        ok = false;
                        break;
                    } else {
                        uf.unify(g[i].from, g[i].to);
                        tree.push_back(g[i]);
                    }
                }
            }
            if (ok) {
                ans.push_back(tree);
            }
        }
        return ans;
    }
};

struct OrigamiReconstructor {
    vector<EnumerateSpanningTree::edge> tree;
    vector<int> facet_types;
    vector<Polygon> facets;
    map<int, map<int, Line>> reflector;
    Graph graph;

    OrigamiReconstructor(const vector<EnumerateSpanningTree::edge> _tree, vector<Polygon> _facets, vector<int> _facet_types, map<int, map<int, Line>> _reflector): tree(_tree), facet_types(_facet_types), reflector(_reflector), graph(_facet_types.size()) {
        facets = vector<Polygon>();
        for (int type : _facet_types) {
            facets.push_back(_facets[type]);
        }
        for (auto edge : tree) {
            graph[edge.from].push_back(Edge(edge.from, edge.to, 1));
            graph[edge.to].push_back(Edge(edge.to, edge.from, 1));
        }
    }

    vector<int> reconstruct_main(int pos, int parent) {
        vector<int> to_reflect;
        for (int i = 0; i < graph[pos].size(); ++i) {
            const Edge edge = graph[pos][i];
            if (edge.dst == parent) {
                continue;
            }
            auto children = reconstruct_main(edge.dst, edge.src);
            // do reflect
            for (int child : children) {
                if (facet_types[pos] != facet_types[edge.dst]) {
                    continue;
                }
                for (int j = 0; j < facets[child].size(); ++j) {
                    Point point = facets[child][j];
                    facets[child].v[j] = reflection(reflector[pos][edge.dst], point);
                }
            }

            to_reflect.insert(to_reflect.end(), children.begin(), children.end());
        }

//        vector<Point> points;
//        for (auto facet : facets) {
//            points.insert(points.end(), facet.begin(), facet.end());
//        }
//
//        cout << points.size() << endl;
//        for (auto facet : facets) {
//            for (int i = 0; i < facet.size(); ++i) {
//                cout << facet[i] << endl;
//            }
//        }
//        cout << facets.size() << endl;
//        int count = 0;
//        for (auto facet : facets) {
//            cout << facet.size();
//            for (int i = 0; i < facet.size(); ++i) {
//                cout << " " << count++;
//            }
//            cout << endl;
//        }

        to_reflect.push_back(pos);
        return to_reflect;
    }

    vector<Polygon> reconstruct(int pos) {
        reconstruct_main(pos, -1);
        return facets;
    }
};


struct ConnectedFacets {
    Graph graph;
    vector<Line> sharing_edge;
    vector<Polygon> facets;
    vector<vector<int>> facet_counts_patterns;
    map<int, map<int, int>> sharing_memo;

    ConnectedFacets(const Graph _graph, const vector<Line> _sharing_edge, const vector<Polygon> _facets, const vector<vector<int>> _facet_counts_patterns): graph(_graph), sharing_edge(_sharing_edge), facets(_facets), facet_counts_patterns(_facet_counts_patterns) {
        for (int i = 0; i < graph.size(); ++i) {
            for (int j = 0; j < graph[i].size(); ++j) {
                Edge edge = graph[i][j];
                sharing_memo[edge.src][edge.dst] = edge.weight;
            }
        }
    }

    tuple<vector<Polygon>, vector<Polygon>, Polygon> reconstructOrigami() {
        for (auto facet_counts : facet_counts_patterns) {
            tuple<vector<Polygon>, vector<Polygon>, Polygon> res = reconstructOrigami(facet_counts);
            Polygon origami = get<2>(res);
            if (origami.size() != 0) {
                return res;
            }
        }
        return make_tuple(vector<Polygon>(), vector<Polygon>(), Polygon());
    }

    string tree_hasher(const vector<EnumerateSpanningTree::edge>& tree, const vector<int>& node_label) {
        vector<vector<int>> g(node_label.size());
        for (auto edge : tree) {
            g[edge.from].push_back(edge.to);
        }

        function<string (int, int)> dfs = [&](int pos, int prev) {
            string ret = to_string(node_label[pos]) + "(";
            for (auto next : g[pos]) {
                if (next == prev) {
                    continue;
                }
                ret += dfs(next, pos);
            }
            ret += ")";
            return ret;
        };

        return dfs(0, -1);
    }

    tuple<vector<Polygon>, vector<Polygon>, Polygon> reconstructOrigami(const vector<int>& facet_counts) {
        // bruteforce all patterns of facet sides.
        // todo: use better enumeration.
        vector<int> node_facet;
        for (int i = 0; i < graph.size(); ++i) {
            for (int j = 0; j < facet_counts[i]; ++j) {
                node_facet.push_back(i);
            }
        }

        if (node_facet.size() > 32) {
            return make_tuple(vector<Polygon>(), vector<Polygon>(), Polygon());
        }
        assert(node_facet.size() <= 32);

        vector<set<int>> adj(graph.size());
        for (int i = 0; i < graph.size(); ++i) {
            for (int j = 0; j < graph[i].size(); ++j) {
                adj[i].insert(graph[i][j].dst);
            }
        }

        set<vector<int>> side_counts; // todo: better hashing
        for (long long mask = 0; mask < (1LL << (node_facet.size() - 1)); ++mask) {
            vector<int> side_counts_hash(graph.size());
            for (int i = 0; i < node_facet.size(); ++i) {
                if ((mask >> i) & 1) {
                    side_counts_hash[node_facet[i]]++;
                }
            }
            if (side_counts.find(side_counts_hash) != side_counts.end()) {
                continue;
            }
            side_counts.insert(side_counts_hash);

            map<int, map<int, Line>> base_reflector;
            vector<pair<int, int>> reflector_choosable;
            vector<vector<Line>> reflector_choice;

            // for each case, reconstruct a spanning tree and test if it represents an origami.
            EnumerateSpanningTree est(node_facet.size());
            for (int src = 0; src < node_facet.size(); ++src) {
                bool src_bit = (mask >> src) & 1;
                for (int dst = 0; dst < node_facet.size(); ++dst) {
                    bool dst_bit = (mask >> dst) & 1;
                    if (src == dst) {
                        continue;
                    }
                    if ((src_bit == dst_bit) && adj[node_facet[src]].find(node_facet[dst]) != adj[node_facet[src]].end()) {
                        base_reflector[src][dst] = sharing_edge[sharing_memo[node_facet[src]][node_facet[dst]]];
                        if (src < dst) {
                            est.add_edge(src, dst);
                        }
                    }
                    if ((src_bit != dst_bit) && node_facet[src] == node_facet[dst]) {
                        // we can try all adj edges as reflector...
                        if (src < dst) {
                            reflector_choosable.push_back(make_pair(src, dst));
                            const Polygon facet = facets[node_facet[src]];
                            vector<Line> choices;
                            for (int i = 0; i < facet.size(); i++) {
                                choices.push_back(Line(facet[i], facet[(i + 1) % facet.size()]));
                            }
                            reflector_choice.push_back(choices);
                            est.add_edge(src, dst);
                        }
                    }
                }
            }

            // build a spanning tree.
            set<string> tree_hashes;
            for (auto tree : est.get_trees()) {
                const string hash = tree_hasher(tree, node_facet);
                if (tree_hashes.find(hash) != tree_hashes.end()) {
                    continue;
                }
                tree_hashes.insert(hash);

                vector<int> reflector_state(reflector_choosable.size());
                while (true) {
                    // reconstruct
                    map<int, map<int, Line>> reflector = base_reflector;
                    for (int i = 0; i < reflector_choosable.size(); ++i) {
                        const int src = reflector_choosable[i].first;
                        const int dst = reflector_choosable[i].second;
                        reflector[src][dst] = reflector_choice[i][reflector_state[i]];
                        reflector[dst][src] = reflector_choice[i][reflector_state[i]];

                    }

                    auto reconstructed_facets = OrigamiReconstructor(tree, facets, node_facet, reflector).reconstruct(0);

                    // test if this is a square (Origami).
                    Polygon origami = isOrigami(reconstructed_facets);
                    if (origami.size() > 0) {
                        // if so, build a solution object.
                        vector<Polygon> original_facets;
                        for (auto facet_type : node_facet) {
                            original_facets.push_back(facets[facet_type]);
                        }
                        return tuple<vector<Polygon>, vector<Polygon>, Polygon>(reconstructed_facets, original_facets, origami);
                    }

                    bool finished = false;
                    for (int i = 0; i <= reflector_choosable.size(); ++i) {
                        if (i == reflector_choosable.size()) {
                            finished = true;
                            break;
                        }
                        reflector_state[i]++;
                        if (reflector_state[i] == reflector_choice[i].size()) {
                            reflector_state[i] = 0;
                        } else {
                            break;
                        }
                    }
                    if (finished) {
                        break;
                    }
                }
            }
        }
        return make_tuple(vector<Polygon>(), vector<Polygon>(), Polygon());
    }

    Polygon isOrigami(vector<Polygon> origami_facets) {
        vector<Point> points;
        for (auto facet : origami_facets) {
            points.insert(points.end(), facet.begin(), facet.end());
        }
//
//        cout << points.size() << endl;
//        for (auto facet : origami_facets) {
//            for (int i = 0; i < facet.size(); ++i) {
//                cout << facet[i] << endl;
//            }
//        }
//        cout << origami_facets.size() << endl;
//        int count = 0;
//        for (auto facet : origami_facets) {
//            cout << facet.size();
//            for (int i = 0; i < facet.size(); ++i) {
//                cout << " " << count++;
//            }
//            cout << endl;
//        }

        auto cmp1 = [](const Point& x, const Point& y) {
            return make_pair(x.x, x.y) < make_pair(y.x, y.y);
        };
        auto cmp2 = [](const Point& x, const Point& y) {
            return make_pair(x.y, -x.x) < make_pair(y.y, -y.x);
        };

        vector<Point> ori(4, points[0]);
        for (auto point : points) {
            ori[0] = min(ori[0], point, cmp1);
            ori[1] = min(ori[1], point, cmp2);
            ori[2] = max(ori[2], point, cmp1);
            ori[3] = max(ori[3], point, cmp2);

        }

        if (abs((ori[2].x - ori[0].x) - (ori[3].y - ori[1].y)) > EPS) { // todo: exact judge when gmp introduced.
            return Polygon();
        }

        for (int i = 0; i < origami_facets.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (PPintersect(origami_facets[i], origami_facets[j])) {
                    return Polygon();
                }
            }
        }

        Polygon origami;
        for (const Point point : ori) {
            origami.add_point(point);
        }
        for (auto point : points) {
            if (contains(origami, point) == OUT_s) {
                return Polygon();
            }
        }
        return origami;
    }
};

struct GeometricGraph {
    Graph graph;
    vector<Point> points;
    GeometricGraph(const Graph _graph, const vector<Point> _points): graph(_graph), points(_points) {

    }

    ConnectedFacets estimateOverlapping() {
        // calculate dual graph
        vector<Polygon> facets;
        vector<Rational> facet_area;
        const int NOT_USED = -1;
        const int OUTER = -2;
        map<int, map<int, int>> edge_usedby;

        for (Edges edges : graph) {
            for (int i = 0; i < edges.size(); ++i) {
                Edge edge = edges[i];
                if (edge_usedby[edge.src].find(edge.dst) != edge_usedby[edge.src].end()) {
                    continue;
                }
                int prev = edge.src;
                int pos = edge.dst;
                vector<Edge> poly;
                poly.push_back(edge);
                while (pos != edge.src) {
                    vector<int> candidate_idx;
                    for (int j = 0; j < graph[pos].size(); ++j) {
                        const Edge candidate_edge = graph[pos][j];
                        if (candidate_edge.dst == prev) {
                            continue;
                        }
                        if (edge_usedby[candidate_edge.src].find(candidate_edge.dst) == edge_usedby[candidate_edge.src].end()) {
                            candidate_idx.push_back(j);
                        }
                    }
                    int next_id = candidate_idx.front();
                    const Point srcp = points[prev];
                    const Point nowp = points[pos];
                    auto cmp = [&](const int x, const int y) {
                        const Point xp = points[graph[pos][x].dst];
                        const Point yp = points[graph[pos][y].dst];

                        const int xc = ccw(srcp, nowp, xp);
                        const int yc = ccw(srcp, nowp, yp);
                        if (xc != yc) {
                            return xc == LEFT || (xc == FRONT && yc == RIGHT);
                        } else {
                            return ccw(nowp, yp, xp) == LEFT;
                        }
                    };

                    for (auto cand : candidate_idx) {
                        next_id = min(next_id, cand, cmp);
                    }

                    poly.push_back(graph[pos][next_id]);
                    prev = pos;
                    pos = graph[pos][next_id].dst;
                }

                Rational signed_area = 0;
                Polygon facet;
                for (int j = 0; j < poly.size(); ++j) {
                    const Edge used_edge = poly[j];
                    signed_area += cross(points[used_edge.src], points[used_edge.dst]) / 2;
                    facet.add_point(points[used_edge.src]);
                }

                int this_facet = signed_area < 0 ? OUTER : facets.size();
                if (this_facet != OUTER) {
                    facets.push_back(facet);
                    facet_area.push_back(signed_area);
                }
                for (int j = 0; j < poly.size(); ++j) {
                    const Edge used_edge = poly[j];
                    edge_usedby[used_edge.src][used_edge.dst] = this_facet;
                }
            }
        }

        Graph dual_graph(facets.size());
        vector<Line> sharing_edge;
        for (Edges edges : graph) {
            for (int i = 0; i < edges.size(); ++i) {
                Edge edge = edges[i];
                int me = edge_usedby[edge.src][edge.dst];
                if (me == OUTER) {
                    continue;
                }
                int they = edge_usedby[edge.dst][edge.src];
                if (they == OUTER) {
                    continue;
                }
                if (me == they) {
                    continue;
                }
                dual_graph[me].push_back(Edge(me, they, sharing_edge.size()));
                sharing_edge.push_back(Line(points[edge.src], points[edge.dst]));
            }
        }

        // estimate overlapping by recursive search.
        vector<vector<int>> facet_counts_list = packing(facet_area, 1);
        sort(facet_counts_list.begin(), facet_counts_list.end(), [](const vector<int>& p, const vector<int>& q) {
            return p.size() < q.size();
        });

        return ConnectedFacets(dual_graph, sharing_edge, facets, facet_counts_list);
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

Point get_point(const string s) {
    for (int j = 0; j < s.size(); j++) {
        if (s[j] == ',') {
            return Point(get_rational(s.substr(0, j)), get_rational(s.substr(j + 1)));
        }
    }
    assert(false);
}

int main() {
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
    auto res = geograph.estimateOverlapping().reconstructOrigami();
    vector<Polygon> tenkaizu = get<0>(res);
    vector<Polygon> original_facets = get<1>(res);
    Polygon origami = get<2>(res);
    if (origami.size() == 0) {
        return 0;
    }

    // origami[0] to (0, 0), origami[1] to (0, 1)
    const Point offset = origami[0];
    const Point rotsrc = origami[1] - offset;

    auto rotate = [&](const Point& p){
        return Point(dot(Point(rotsrc.x, rotsrc.y), p), dot(Point(-rotsrc.y, rotsrc.x), p));
    };

    map<Point, int> uniq_points;
    vector<Point> idx2point;
    vector<Point> dests;
    vector<vector<int>> facets;
    for (int i = 0; i < tenkaizu.size(); ++i) {
        auto facet = tenkaizu[i];
        facets.push_back(vector<int>());
        for (int j = 0; j < facet.size(); ++j) {
            Point &point = facet.v[j];
            point = rotate(point - offset);
            if (uniq_points.find(point) == uniq_points.end()) {
                const int idx = uniq_points.size();
                uniq_points[point] = idx;
                idx2point.push_back(point);
                dests.push_back(original_facets[i][j]);
            }
            facets.back().push_back(uniq_points[point]);
        }
    }

    cout << uniq_points.size() << endl;
    for (auto point : idx2point) {
        cout << point << endl;
    }

    cout << facets.size() << endl;
    for (auto facet : facets) {
        cout << facet.size();
        for (auto idx : facet) {
            cout << " " << idx;
        }
        cout << endl;
    }

    for (auto point : dests) {
        cout << point << endl;
    }

    return 0;
}
