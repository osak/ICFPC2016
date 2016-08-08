#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <iomanip>
#include <algorithm>

using namespace std;

class Int {
    public:
    
    Int(long long num = 0) {
        digit.clear();
        sign = 1;
        
        if (num == 0) {
            digit.push_back(0);
        } else {
            if (num < 0) {
                sign = -1;
                num = -num;
            }
            
            while (num) {
                digit.push_back(num % 10000);
                num /= 10000;
            }
        }
    }
    
    ~Int() {}
    
    Int& operator=(const Int& num) {
        this->digit = num.digit;
        this->sign = num.sign;
        
        return *this;
    }
    
    Int& operator=(long long num) {
        return *this = (Int)num;
    }
    
    const bool operator<(const Int& num) const {
        if (sign != num.sign) return sign == -1;
        
        if (digit.size() != num.digit.size()) {
            if (sign == 1) {
                return digit.size() < num.digit.size();
            } else {
                return digit.size() > num.digit.size();
            }
        }
        
        for (int i = digit.size() - 1; i >= 0; i--) {
            if (digit[i] != num.digit[i]) {
                if (sign == 1) {
                    return digit[i] < num.digit[i];
                } else {
                    return digit[i] > num.digit[i];
                }
            }
        }
        
        return false;
    }
    
    const bool operator>(const Int& num) const {
        return num < *this;
    }
    
    const bool operator<=(const Int& num) const {
        return !(num < *this);
    }
    
    const bool operator>=(const Int& num) const {
        return !(*this < num);
    }
    
    const bool operator!=(const Int& num) const {
        return *this < num || num < *this;
    }
    
    const bool operator==(const Int& num) const {
        return !(*this < num) && !(num < *this);
    }
    
    friend istream& operator>>(istream& is, Int& num) {
        string s;
        
        is >> s;
        
        num.digit.clear();
        
        if (s[0] == '-') {
            num.sign = -1;
        } else {
            num.sign = 1;
        }
        
        for (int i = s.size() - 1; i >= 0; i -= 4) {
            int sum = 0, tmp = 1;
            
            for (int j = 0; j < 4; j++) {
                if (i - j < 0) break;
                
                if (!(s[i - j] >= '0' && s[i - j] <= '9')) break;
                
                sum += (s[i - j] - '0') * tmp;
                tmp *= 10;
            }
            
            num.digit.push_back(sum);
        }
        
        num.remove();
        
        return is;
    }
    
    friend ostream& operator<<(ostream& os, const Int& num) {
        if (num.sign == -1) os << "-";
        
        os << num.digit[num.digit.size() - 1];
        
        for (int i = (int)num.digit.size() - 2; i >= 0; i--) os << setw(4) << setfill('0') << num.digit[i];
        
        return os;
    }
    
    Int operator-() const {
        Int num = *this;
        
        if (digit.size() > 1 || digit[0] > 0 || sign == -1) num.sign *= -1;
        
        return num;
    }
    
    Int operator++(int) {
        Int ans = *this;
        
        *this = *this + 1;
        
        return ans;
    }
    
    Int& operator++() {
        return *this = *this + 1;
    }
    
    Int operator--(int) {
        Int ans = *this;
        
        *this = *this - 1;
        
        return ans;
    }
    
    Int& operator--() {
        return *this = *this - 1;
    }
    
    const Int operator+(const Int& num) const {
        if (this->sign == num.sign) {
            Int ans = add(*this, num);
            
            ans.sign = this->sign;
            
            ans.remove();
            
            return ans;
        } else if (cmp_abs(*this, num)) {
            Int ans = sub(num, *this);
            
            if (this->sign == 1) ans.sign = -1;
            
            ans.remove();
            
            return ans;
        } else {
            Int ans = sub(*this, num);
            
            if (num.sign == 1) ans.sign = -1;
            
            ans.remove();
            
            return ans;
        }
    }
    
    const Int operator-(const Int& num) const {
        return *this + (-num);
    }
    
    const Int operator*(const Int& num) const {
        Int ans = mul(*this, num);
        
        ans.sign = this->sign * num.sign;
        
        ans.remove();
        
        return ans;
    }
    
    const Int operator/(const Int& num) const {
        Int ans = div(*this, num);
        
        ans.sign = this->sign * num.sign;
        
        ans.remove();
        
        return ans;
    }
    
    const Int operator%(const Int& num) const {
        return *this - *this / num * num;
    }
    
    const Int operator^(const Int& num) const {
        Int ans = pow(*this, num);
        
        if (this->sign == -1 && (num.digit[0] & 1) == 1) ans.sign = -1;
        
        return ans;
    }
    
    Int operator+=(const Int& num) {
        return *this = *this + num;
    }
    
    Int operator-=(const Int& num) {
        return *this = *this - num;
    }
    
    Int operator*=(const Int& num) {
        return *this = *this * num;
    }
    
    Int operator/=(const Int& num) {
        return *this = *this / num;
    }
    
    Int operator%=(const Int& num) {
        return *this = *this % num;
    }
    
    Int operator^=(const Int& num) {
        return *this = *this ^ num;
    }
    
    Int fact(void) const {
        Int ans = *this;
        long long c, p;
        
        ans.shift();
        ans++;
        
        c = ans.digit[0];
        if (ans.digit.size() > 1) c += ans.digit[1] * 10000;
        p = c * c - 1;
        if ((digit[0] & 1) == 0) c--;
        
        for (int i = 1; i < c; i++) {
            ans *= p;
            p -= i * 2 + 1;
        }
        
        return ans;
    }
    
    const Int sqrt() const {
        Int l = 0, r = *this + 1, m = (l + r) / 2;
        
        while (r - l > 1) {
            Int m2 = m * m;
            
            if (m2 == *this) return m;
            
            if (m2 > *this) {
                r = m;
                m = (l + r) / 2;
            } else {
                l = m;
                m = (l + r) / 2;
            }
        }
        
        return -1;
    }
    
    private:
    
    vector <int> digit;
    int sign;
    
    void remove(void) {
        for (int i = digit.size() - 1; i > 0; i--) {
            if (digit[i] > 0) break;
            
            digit.pop_back();
        }
        
        if (digit.size() == 1 && digit[0] == 0 && sign == -1) sign = 1;
    }
    
    void shift(void) {
        int c = 0;
        
        for (int i = digit.size() - 1; i >= 0; i--) {
            digit[i] += c * 10000;
            
            c = digit[i] % 2;
            digit[i] /= 2;
        }
        
        remove();
    }
    
    const bool cmp_abs(const Int& a, const Int& b) const {
        if (a.digit.size() != b.digit.size()) return a.digit.size() < b.digit.size();
        
        for (int i = a.digit.size() - 1; i >= 0; i--) {
            if (a.digit[i] != b.digit[i]) return a.digit[i] < b.digit[i];
        }
        
        return false;
    }
    
    Int add(const Int& a, const Int& b) const {
        int c = 0;
        Int ans;
        
        ans.digit.clear();
        
        for (int i = 0; i < (int)a.digit.size() || i < (int)b.digit.size(); i++) {
            int tmp = c;
            
            if (i < (int)a.digit.size()) tmp += a.digit[i];
            if (i < (int)b.digit.size()) tmp += b.digit[i];
            
            c = tmp / 10000;
            ans.digit.push_back(tmp % 10000);
        }
        
        if (c > 0) ans.digit.push_back(c);
        
        return ans;
    }
    
    Int sub(const Int& a, const Int& b) const {
        int i;
        Int ans = a;
        
        ans.sign = 1;
        
        for (i = 0; i < (int)b.digit.size(); i++) {
            ans.digit[i] -= b.digit[i];
            
            if (ans.digit[i] < 0) {
                ans.digit[i + 1]--;
                ans.digit[i] += 10000;
            }
        }
        
        for (; i < (int)ans.digit.size(); i++) {
            if (ans.digit[i] >= 0) break;
            
            ans.digit[i + 1]--;
            ans.digit[i] += 10000;
        }
        
        return ans;
    }
    
    Int mul(const Int& a, const Int& b) const {
        Int ans;
        
        ans.digit = vector<int>(a.digit.size() + b.digit.size(), 0);
        
        for (int i = 0; i < (int)ans.digit.size(); i++) {
            for (int j = max(0, i - (int)b.digit.size() + 1); j <= i && j < (int)a.digit.size(); j++) {
                ans.digit[i] += a.digit[j] * b.digit[i - j];
                
                if (ans.digit[i] >= 10000) {
                    ans.digit[i + 1] += ans.digit[i] / 10000;
                    ans.digit[i] %= 10000;
                }
            }
        }
        
        return ans;
    }
    
    Int div(const Int& a, const Int& b) const {
        Int ans, x = a, y = b;
        int c = a.digit.size() - b.digit.size();
        
        if (cmp_abs(a, b)) return 0;
        
        x.sign = y.sign = 1;
        
        ans.digit = vector<int>(x.digit.size() - y.digit.size() + 1, 0);
        
        y.digit.insert(y.digit.begin(), x.digit.size() - y.digit.size(), 0);
        
        if (y > x) {
            y.digit.erase(y.digit.begin());
            
            c--;
        }
        
        for (int i = c; i >= 0; i--) {
            int p = x.digit[x.digit.size() - 1], l, r, m;
            Int q;
            
            if (x < y) {
                y.digit.erase(y.digit.begin());
                
                continue;
            }
            
            if (x.digit.size() != y.digit.size()) p = p * 10000 + x.digit[x.digit.size() - 2];
            
            l = p / (y.digit[y.digit.size() - 1] + 1);
            r = p / y.digit[y.digit.size() - 1] + 1;
            m = (l + r) / 2;
            
            while (r - l > 1) {
                if (y * m > x) {
                    r = m;
                    m = (l + r) / 2;
                } else {
                    l = m;
                    m = (l + r) / 2;
                }
            }
            
            ans.digit[i] = l;
            
            x -= y * l;
            y.digit.erase(y.digit.begin());
        }
        
        return ans;
    }
    
    Int pow(const Int& a, const Int& b) const {
        Int ans = 1, x = a, y = b;
        
        if (a == 0) return 0;
        
        if (b.sign == -1) {
            if (a == 1 && b == -1) {
                return 1;
            } else {
                return 0;
            }
        }
        
        while (y.digit.size() > 1 || y.digit[0] > 0) {
            if (y.digit[0] & 1) ans *= x;
            
            x *= x;
            y.shift();
        }
        
        return ans;
    }
};

const bool operator<(int a, const Int& b)
{
    return (Int)a < b;
}

const bool operator>(int a, const Int& b)
{
    return (Int)a > b;
}

const bool operator<=(int a, const Int& b)
{
    return (Int)a <= b;
}

const bool operator>=(int a, const Int& b)
{
    return (Int)a >= b;
}

const bool operator!=(int a, const Int& b)
{
    return (Int)a != b;
}

const bool operator==(int a, const Int& b)
{
    return (Int)a == b;
}

Int operator+(int a, const Int& b)
{
    return (Int)a + b;
}

Int operator-(int a, const Int& b)
{
    return (Int)a - b;
}

Int operator*(int a, const Int& b)
{
    return (Int)a * b;
}

Int operator/(int a, const Int& b)
{
    return (Int)a / b;
}

Int operator%(int a, const Int& b)
{
    return (Int)a % b;
}

Int operator^(int a, const Int& b)
{
    return (Int)a ^ b;
}

Int gcd(const Int &a, const Int &b) {
    if (b == 0) {
        return a;
    } else {
        return gcd(b, a % b);
    }
}

class Rational {
    public:
    
    Rational(const int p = 0) : p(p), q(1) {
    }
    
    Rational(const Int &p) : p(p), q(1) {
    }
    
    Rational(const Int &p, const Int &q) : p(p), q(q) {
        normalize();
    }
    
    const bool operator<(const Rational& num) const {
        return p * num.q < q * num.p;
    }
    
    const bool operator>(const Rational& num) const {
        return num < *this;
    }
    
    const bool operator<=(const Rational& num) const {
        return !(num < *this);
    }
    
    const bool operator>=(const Rational& num) const {
        return !(*this < num);
    }
    
    const bool operator!=(const Rational& num) const {
        return *this < num || num < *this;
    }
    
    const bool operator==(const Rational& num) const {
        return !(*this < num) && !(num < *this);
    }
    
    friend ostream& operator<<(ostream& os, const Rational& num) {
        if (num.q == 1) {
            os << num.p;
        } else {
            os << num.p << "/" << num.q;
        }
        return os;
    }
    
    Rational operator-() const {
        return Rational(-p, q);
    }
    
    const Rational operator+(const Rational& num) const {
        return Rational(p * num.q + q * num.p, q * num.q);
    }
    
    const Rational operator-(const Rational& num) const {
        return Rational(p * num.q - q * num.p, q * num.q);
    }
    
    const Rational operator*(const Rational& num) const {
        return Rational(p * num.p, q * num.q);
    }
    
    const Rational operator/(const Rational& num) const {
        return Rational(p * num.q, q * num.p);
    }
    
    Rational operator+=(const Rational& num) {
        return *this = *this + num;
    }
    
    Rational operator-=(const Rational& num) {
        return *this = *this - num;
    }
    
    Rational operator*=(const Rational& num) {
        return *this = *this * num;
    }
    
    Rational operator/=(const Rational& num) {
        return *this = *this / num;
    }
    
    const Rational sqrt() const {
        Int pp = p.sqrt();
        if (pp == -1) return -1;
        Int qq = q.sqrt();
        if (qq == -1) return -1;
        return Rational(pp, qq);
    }
    
    private:
    Int p, q;
    
    void normalize() {
        if (q < 0) {
            p *= -1;
            q *= -1;
        }
        Int g = gcd(p < 0 ? -p : p, q);
        if (g == 0) {
            p = 0;
            q = 1;
        } else {
            p /= g;
            q /= g;
        }
    }
};

Rational abs(const Rational &x) {
    return x < 0 ? -x : x;
}

class Point {
    public:
    Rational x;
    Rational y;
    
    Point(Rational x = 0, Rational y = 0) : x(x), y(y) {}
    bool operator<(const Point& p) const {
        return abs(x - p.x) > 0 ? x < p.x : y < p.y;
    }
    bool operator==(const Point& p) const {
        return abs(x - p.x) < 0 && abs(y - p.y) < 0;
    }
    Point operator+(const Point& p) const {
        return Point(x + p.x, y + p.y);
    }
    Point operator-(const Point& p) const {
        return Point(x - p.x, y - p.y);
    }
    Point operator*(Rational r) const {
        return Point(x * r, y * r);
    }
    Point operator/(Rational r) const {
        return Point(x / r, y / r);
    }
    
    friend ostream& operator<<(ostream& os, const Point& num) {
        os << num.x << "," << num.y;
        return os;
    }
};

class Line {
    public:
    Point p1;
    Point p2;
    
    Line(const Point& p1 = Point(), const Point& p2 = Point()) : p1(p1), p2(p2) {}
    Point get_vector() const {
        return p2 - p1;
    }
};

inline Rational dist(const Point& p1, const Point& p2) {
    Point p = p2 - p1;
    Rational d2 = p.x * p.x + p.y * p.y;
    return d2.sqrt();
}

enum CCW {
    LEFT = 1,
    RIGHT = -1,
    FRONT = 2,
    BACK = -2,
    ON = 0
};

inline CCW ccw(const Point& p1, const Point& p2, const Point& p3) {
    Point v = p2 - p1;
    Point w = p3 - p1;
    Rational r = v.x * w.y - v.y * w.x;
    if (r > 0) return LEFT;
    if (r < 0) return RIGHT;
    Rational t = (v.x * w.x + v.y * w.y) / (v.x * v.x + v.y * v.y);
    if (t > 1) return FRONT;
    if (t < 0) return BACK;
    return ON;
}

inline Rational get_ratio(const Line& l, const Point& p) {
    Point v = l.get_vector();
    Point w = p - l.p1;
    return (v.x * w.x + v.y * w.y) / (v.x * v.x + v.y * v.y);    
}

inline Point projection(const Line& l, const Point& p) {
    return l.p1 + l.get_vector() * get_ratio(l, p);
}

inline Point reflection(const Line& l, const Point& p) {
    return p + (projection(l, p) - p) * 2;
}

inline Rational cross(const Point& p1, const Point& p2) {
    return p1.x * p2.y - p1.y * p2.x;
}

inline Point crosspoint(const Line& l1, const Line& l2) {
    Rational a = cross(l1.get_vector(), l2.get_vector());
    Rational b = cross(l1.get_vector(), l1.p2 - l2.p1);
    
    if (abs(a) == 0 && abs(b) == 0) return l2.p1;
    return l2.p1 + l2.get_vector() * b / a;
}

class Polygon {
    public:
    vector <Point> v;
    vector <Line> history;
    
    void add_point(const Point& p) {
        v.push_back(p);
    }
    
    void mirror(const Line& l) {
        for (int i = 0; i < v.size(); i++) v[i] = reflection(l, v[i]);
        reverse(v.begin(), v.end());
        history.push_back(l);
    }
};

class Matrix {
    public:
    Point origin;
    Rational x;
    Rational y;
    
    Matrix() {}
    
    Matrix(const Point& origin, const Rational& x, const Rational& y) : origin(origin), x(x), y(y) {}
    
    const Point convert(const Point& p) const {
        Point p2 = p - origin;
        return Point(p2.x * x - p2.y * y, p2.x * y + p2.y * x);
    }
    
    const Point reconvert(const Point& p) const {
        return Point(p.x * x + p.y * y, -p.x * y + p.y * x) + origin;
    }
};

Rational get(const string &s) {
    int i;
    
    for (i = 0; i < s.size(); i++) {
        if (s[i] == '/') {
            Int p, q;
            stringstream ss1, ss2;
            
            ss1 << s.substr(0, i);
            ss1 >> p;
            ss2 << s.substr(i + 1);
            ss2 >> q;
            
            return Rational(p, q);
        }
    }
    
    Int p;
    stringstream ss;
    
    ss << s;
    ss >> p;
    
    return Rational(p);
}

Point get_point(const string &s) {
    int i;
    
    for (i = 0; i < s.size(); i++) {
        if (s[i] == ',') return Point(get(s.substr(0, i)), get(s.substr(i + 1)));
    }
    
    return Point();
}

vector <Polygon> cut(Polygon& p, const Line& l) {
    int n = p.v.size(), i;
    bool left = false, right = false;
    vector <Polygon> ps;
    
    for (i = 0; i < n; i++) {
        CCW c = ccw(l.p1, l.p2, p.v[i]);
        
        if (c == LEFT) left = true;
        if (c == RIGHT) right = true;
    }
    
    if (right == false) {
        ps.push_back(p);
    } else if (left == false) {
        p.mirror(l);
        ps.push_back(p);
    } else {
        Polygon p1, p2;
        
        p1.history = p2.history = p.history;
        
        for (i = 0; i < n; i++) {
            CCW c1 = ccw(l.p1, l.p2, p.v[i]);
            CCW c2 = ccw(l.p1, l.p2, p.v[(i + 1) % n]);
            
            if (c1 != RIGHT) p1.add_point(p.v[i]);
            if (c1 != LEFT) p2.add_point(p.v[i]);
            if ((c1 == LEFT && c2 == RIGHT) || (c1 == RIGHT && c2 == LEFT)) {
                Point pc = crosspoint(l, Line(p.v[i], p.v[(i + 1) % n]));
                
                p1.add_point(pc);
                p2.add_point(pc);
            }
        }
        
        p2.mirror(l);
        ps.push_back(p1);
        ps.push_back(p2);
    }
    
    return ps;
}

string output(const vector <Polygon>& ps, const Matrix& m) {
    int i, j, k;
    stringstream ss;
    vector <vector <Point> > v(ps.size());
    map <Point, pair<Point, int> > mp;
    map <Point, pair<Point, int> >::iterator it;
    
    for (i = 0; i < ps.size(); i++) {
        
        for (j = 0; j < ps[i].v.size(); j++) {
            Point p = ps[i].v[j];
            
            for (k = (int)ps[i].history.size() - 1; k >= 0; k--) p = reflection(ps[i].history[k], p);
            
            mp[p] = make_pair(ps[i].v[j], 0);
            v[i].push_back(p);
        }
    }
    
    for (it = mp.begin(), i = 0; it != mp.end(); it++, i++) it->second.second = i;
    
    ss << mp.size() << endl;
    for (it = mp.begin(); it != mp.end(); it++) ss << m.convert(it->first) << endl;
    
    ss << ps.size() << endl;
    for (i = 0; i < ps.size(); i++) {
        ss << ps[i].v.size();
        for (j = 0; j < v[i].size(); j++) ss << " " << mp[v[i][j]].second;
        ss << endl;
    }
    
    for (it = mp.begin(); it != mp.end(); it++) ss << it->second.first << endl;
    
    return ss.str();
}

void next(const vector <Polygon>& ps, const Matrix& m) {
    int i, j;
    
    cout << ps.size() << endl;
    for (i = 0; i < ps.size(); i++) {
        cout << ps[i].v.size() << endl;
        
        for (j = 0; j < ps[i].v.size(); j++) cout << ps[i].v[j] << endl;
    }
    
    for (i = 0; i < ps.size(); i++) {
        cout << ps[i].history.size() << endl;
        
        for (j = 0; j < ps[i].history.size(); j++) cout << ps[i].history[j].p1 << " " << ps[i].history[j].p2 << endl;
    }
    
    cout << m.origin << " " << m.x << " " << m.y << endl;
}

void init(void) {
    int i;
    string s, t;
    
    cin >> s >> t;
    
    Line l = Line(get_point(s), get_point(t));
    
    Polygon p;
    p.add_point(Point(0, 0));
    p.add_point(Point(1, 0));
    p.add_point(Point(1, 1));
    p.add_point(Point(0, 1));
    
    Rational d = dist(l.p1, l.p2);
    
    Rational x = (l.p2.x - l.p1.x) / d;
    Rational y = (l.p1.y - l.p2.y) / d;
    
    Matrix m = Matrix(l.p1, x, y);
    
    for (i = 0; i < 4; i++) p.v[i] = m.reconvert(p.v[i]);
    
    vector <Polygon> ps;
    ps.push_back(p);
    
    next(ps, m);
}

vector <Polygon> get_polygon(void) {
    int n, i, j;
    vector <Polygon> ps;
    
    cin >> n;
    
    for (i = 0; i < n; i++) {
        int m;
        Polygon p;
        
        cin >> m;
        
        for (j = 0; j < m; j++) {
            string s;
            
            cin >> s;
            
            p.add_point(get_point(s));
        }
        
        ps.push_back(p);
    }
    
    for (i = 0; i < n; i++) {
        int m;
        
        cin >> m;
        
        for (j = 0; j < m; j++) {
            string s, t;
            
            cin >> s >> t;
            
            ps[i].history.push_back(Line(get_point(s), get_point(t)));
        }
    }
    
    return ps;
}

Matrix get_matrix(void) {
    string s, x, y;
    
    cin >> s >> x >> y;
    
    return Matrix(get_point(s), get(x), get(y));
}

void fold(vector <Polygon>& ps, const Matrix& m) {
    int i, j;
    string s, t;
    
    cin >> s >> t;
    
    Line l = Line(get_point(s), get_point(t));
    
    int n;
    vector <int> v(ps.size(), 0);
    vector <Polygon> ps2;
    
    cin >> n;
    
    for (i = 0; i < n; i++) {
        int x;
        
        cin >> x;
        
        v[x] = 1;
    }
    
    for (i = 0; i < ps.size(); i++) {
        if (v[i] == 0) {
            ps2.push_back(ps[i]);
        } else {
            vector <Polygon> nps = cut(ps[i], l);
            
            for (j = 0; j < nps.size(); j++) ps2.push_back(nps[j]);
        }
    }
    
    next(ps2, m);
}

int main() {
    string type;
    
    cin >> type;
    
    if (type == "init") {
        init();
    } else {
        vector <Polygon> ps = get_polygon();
        Matrix m = get_matrix();
        
        if (type == "end") {
            cout << output(ps, m);
        } else {
            fold(ps, m);
        }
    }
    
    return 0;
}
