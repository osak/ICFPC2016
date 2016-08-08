/**
 * Created by keitak on 8/5/16.
 */
public class Point {
    Fraction x, y;
    static Point origin = new Point(Fraction.ZERO, Fraction.ZERO);
    static Point p[][] = {
            {new Point(Fraction.ZERO, Fraction.ZERO),
            new Point(Fraction.ZERO, Fraction.ONE)},
            {new Point(Fraction.ONE, Fraction.ZERO),
            new Point(Fraction.ONE, Fraction.ONE)}
    };

    public Point(Fraction x, Fraction y) {
        this.x = x;
        this.y = y;
    }

    public Point minus() {
        return new Point(x.minus(), y.minus());
    }

    public Point sub(Point p) {
        return new Point(x.sub(p.x), y.sub(p.y));
    }

    public Point add(Point p) {
        return new Point(x.add(p.x), y.add(p.y));
    }

    public String toString() {
        return x + "," + y;
    }

    @Override
    public int hashCode() {
        return x.hashCode() * 31  + y.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        Point p = (Point)obj;

        return x.equals(p.x) && y.equals(p.y);
    }
}
