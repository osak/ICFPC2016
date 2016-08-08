/**
 * Created by keitak on 8/5/16.
 */
public class Edge {
    Point s, t;

    public Edge(Point s, Point t) {
        this.s = s;
        this.t = t;
    }

    Edge originS() {
        return new Edge(Point.origin, t.sub(s));
    }

    @Override
    public String toString() {
        return s + "," + t;
    }

    @Override
    public int hashCode() {
        return s.hashCode() + t.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        Edge e = (Edge)obj;
        return s.equals(e.s) && t.equals(e.t) ||
                s.equals(e.t) && t.equals(e.s);
    }
}
