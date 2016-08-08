#include <iostream>
#include <vector>
#include <string>
#include <tuple>

#define double long double

using namespace std;

using P = pair<double, double>;

pair<string, string> split(string s, char token) {
    for (int i = 0; i < s.size(); ++i) {
        if(s[i] == token) return make_pair(s.substr(0, i), s.substr(i+1));
    }
    return make_pair(s, "1");
}

double waru(string s) {
    auto x = split(s, '/');
    return (double) stod(x.first) / stod(x.second);
}

double f(double x) {
    double base = 100;
    double mul = 300;
    return base + mul*x;
}

string nf(double x) {
    return to_string(f(x));
}

string rf(double x) {
   return to_string(500 - f(x)); 
}

string line(P p1, P p2) {
    return "line(" + nf(p1.first) + ',' + rf(p1.second) + ',' + nf(p2.first) + ',' + rf(p2.second) + ')'; 
}

void output(vector<P> &v) {
    int n = v.size();
    for (int i = 0; i < n; ++i) {
        cout << line(v[i], v[(i+1)%n]) << endl;
    }
    return;
}

void adhocMove(vector<P> &v) {
    double minX = 1e100;
    double minY = 1e100;
    int n = v.size();
    for (int i = 0; i < n; ++i) {
        minX = min(minX, v[i].first);
        minY = min(minY, v[i].second);
    }
    for (int i = 0; i < n; ++i) {
        v[i].first -= minX;
        v[i].second -= minY;
    }
}

// comma separated
P getP(string s) {
    string pa, pb;
    tie(pa, pb) = split(s, ',');
    return make_pair(waru(pa), waru(pb));
}

int main() {
    int np;
    cin >> np;
    vector<vector<pair<double, double>>> poly(np);
    for (int i = 0; i < np; ++i) {
        int n;
        cin >> n;
        vector<pair<double, double>> a(n);
        cin.ignore();
        for (int j = 0; j < n; ++j) {
            string s;
            getline(cin, s);
            a[j] = getP(s);
        }
    }
    int sk;
    cin >> sk;
    cin.ignore();
    for (int i = 0; i < sk; ++i) {
        string s;
        getline(cin, s);
        string ps, pt;
        tie(ps, pt) = split(s, ' ');
        vector<P> v = {getP(ps), getP(pt)};
        adhocMove(v);
        output(v);
    }
}

