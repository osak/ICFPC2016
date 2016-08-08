#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <iomanip>
#include <algorithm>

using namespace std;

class Int {
    public:
    
    string to_string() {
        string s;
        if (sign == -1) s = "-";
        s += std::to_string(digit[digit.size() - 1]);
        
        for (int i = (int)digit.size() - 2; i >= 0; i--) {
            string t = "0000" + std::to_string(digit[i]);
            s += t.substr(t.size() - 4);
        }
        return s;
    }

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

    long double ld() {
        normalize();
        return stold(p.to_string()) / stold(q.to_string());
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

pair<string, string> split(string s, char token) {
    for (int i = 0; i < s.size(); ++i) {
        if(s[i] == token) return make_pair(s.substr(0, i), s.substr(i+1));
    }
    return make_pair(s, "1");
}

using P = pair<Rational, Rational>;

void text(string s, P a) {
    cout << "text('" << s << "'," << a.first.ld() << ',' << a.second.ld() << ')' << endl;
}

void line(P &a, P &b) {
    cout << "line(" << a.first.ld() << ',' << a.second.ld() << ',' << b.first.ld() << ',' << b.second.ld() << ')' << endl;
}

void point(P a, int size = 5) {
    cout << "circle(" << a.first.ld() << ',' << a.second.ld() << "," << size << ")" << endl;
}

void color(string col) {
    cout << "color('" << col << "')" << endl;
}

void hsl(int x) {
    cout << "hsl(" << (x%360) << ",'100%','60%')" << endl;
}

void polygon(vector<P> v) {
    int n = v.size();
    cout << "polyfill([";
    for (int i = 0; i < n; ++i) {
        cout << v[i].first.ld() << (i == n-1 ? "" : ",");
    }
    cout << "],[";
    for (int i = 0; i < n; ++i) {
        cout << v[i].second.ld() << (i == n-1 ? "" : ",");
    }
    cout << "])" << endl;
}

vector<P> shift(vector<P> v) {
    int n = v.size();
    Rational minX;
    Rational minY;
    bool f = true;
    for (int i = 0; i < n; ++i) {
        minX = (f) ? v[i].first : min(minX, v[i].first);
        minY = (f) ? v[i].second : min(minY, v[i].second);
        f = false;
    }
    int base = 100, mul = 300;
    for (int i = 0; i < n; ++i) {
        // cout << "before" << endl;
        // cout << v[i].first << ' ' << v[i].second << endl;
        v[i].first -= minX;
        v[i].second -= minY;
        v[i].first *= mul;
        v[i].second *= mul;
        v[i].first += base;
        v[i].second = -v[i].second + 450;
        // cout << "after" << endl;
        // cout << v[i].first << ' ' << v[i].second << endl;
        // cout << "after-double" << endl;
        // cout << v[i].first.ld() << ' ' << v[i].second.ld() << endl;
    }
    return v;
}

vector<P> shiftX(vector<P> v, int x = 400) {
    int n = v.size();
    for (int i = 0; i < n; ++i) {
        // cout << "before" << endl;
        // cout << v[i].first << ' ' << v[i].second << endl;
        // cout << "before-double" << endl;
        // cout << v[i].first.ld() << ' ' << v[i].second.ld() << endl;
        v[i].first += x;
        // cout << "after" << endl;
        // cout << v[i].first << ' ' << v[i].second << endl;
        // cout << "after-double" << endl;
        // cout << v[i].first.ld() << ' ' << v[i].second.ld() << endl;
    }
    return v;
}


int main() {
    int n;
    cin >> n;
    vector<P> source(n);
    for (int i = 0; i < n; ++i) {
        string s;
        cin >> s;
        auto p = split(s, ',');
        source[i] = make_pair(get(p.first), get(p.second));
    }
    int k;
    cin >> k;
    vector<vector<int>> facets(k);
    for (int i = 0; i < k; ++i) {
        int l;
        cin >> l;
        for (int j = 0; j < l; ++j) {
            int x;
            cin >> x;
            facets[i].push_back(x);
        }
    }
    vector<P> dest(n);
    for (int i = 0; i < n; ++i) {
        string s;
        cin >> s;
        auto p = split(s, ',');
        dest[i] = make_pair(get(p.first), get(p.second));
    }

    source = shift(source);
    auto orgDest = dest;
    dest = shift(dest);
    dest = shiftX(dest);

    vector<vector<P>> pfacets(k);
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < facets[i].size(); ++j) {
            pfacets[i].push_back(source[facets[i][j]]);
        }
    }

    vector<vector<P>> pfacetsd(k);
    for (int i = 0; i < k; ++i) {
        for (int j = 0; j < facets[i].size(); ++j) {
            pfacetsd[i].push_back(dest[facets[i][j]]);
        }
    }

    const int colorStep = 70;
    // draw facets
    for (int i = 0; i < k; ++i) {
        //color(iro[i%iro.size()]);
        hsl(i*colorStep);
        polygon(pfacets[i]);
    }

    // draw points
    color("black");
    for (int i = 0; i < n; ++i) {
        text(to_string(i), source[i]);
    }

    for (int i = 0; i < k; ++i) {
        //color(iro[i%iro.size()]);
        hsl(i*colorStep);
        polygon(pfacetsd[i]);
    }

    color("black");
    for (int i = 0; i < n; ++i) {
        text(to_string(i), dest[i]);
        //text("" + orgDest[i].first + " " + orgDest[i].first + "")
    }

    return 0;
}
