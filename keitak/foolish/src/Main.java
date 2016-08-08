import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
	    Scanner sc = new Scanner(System.in);
        int N = sc.nextInt();
        ArrayList<Point> plist = new ArrayList<>();
        for (int i = 0; i < N; i++) {
            int M = sc.nextInt();
            for (int j = 0; j < M; j++) {
                String line = sc.next();
                plist.add(formPoint(line));
            }
        }

        PrintWriter pw = new PrintWriter(System.out);

        if (args.length > 0 && args[0].equals("a")) {
            Point points[] = new Point[plist.size()];
            points = plist.toArray(points);
            output(pw, points);
        } else {
            //Ignore skeleton
            Point points[] = new Point[plist.size()];
            points = plist.toArray(points);
            outputWithRotate(pw, points, getEdges(points)[0]);
        }
        pw.close();
    }

    static void outputWithRotate(PrintWriter pw, Point[] ps, Edge edge) {
        Edge origin = edge.originS();
        Point base = origin.t;
        if (base.y.equals(Fraction.ZERO)) {
            output(pw, ps);
            return;
        }

        Point s = edge.s;
        ps = adder(ps, s.minus());//shift
        ps = rotator(true, ps, base);
        Answer ans = answer(ps);

        ans = new Answer(ans.source, ans.facets, rotator(false, ans.destination, base));
        ans = new Answer(ans.source, ans.facets, adder(ans.destination, s));

        outputAnswer(pw, ans);
    }

    static void output(PrintWriter pw, Point[] ps) {
        Answer ans = answer(ps);
        outputAnswer(pw, ans);
    }

    static Point[] rotator(boolean b, Point[] ps, Point base) {
        Point[] res = new Point[ps.length];
        for (int i = 0; i < ps.length; i++) {
            res[i] = rotate(b, ps[i], base);
        }
        return res;
    }

    static Point rotate(boolean b, Point p, Point base) {
        Fraction[][] MX;
        Fraction X = base.x;
        Fraction Y = base.y.minus();

        if (b) {
            MX = new Fraction[][]{
                    {X, Y.minus()},
                    {Y, X}
            };
        } else {
            MX = new Fraction[][]{
                    {X, Y},
                    {Y.minus(), X}
            };
        }

        return rotate(MX, p);
    }

    static Point rotate(Fraction[][] mx, Point p) {
        Fraction x = mx[0][0].mul(p.x).add(mx[0][1].mul(p.y));
        Fraction y = mx[1][0].mul(p.x).add(mx[1][1].mul(p.y));
        return new Point(x, y);
    }

    static Edge[] getEdges(Point[] ps) {
        Edge[] res = new  Edge[ps.length];
        for (int i = 0; i < ps.length; i++) {
            res[i] = new Edge(ps[i], ps[(i+1)%ps.length]);
        }
        return res;
    }



    static Point formPoint(String line) {
        String xy[] = line.split(",");
        return new Point(formFraction(xy[0]), formFraction(xy[1]));
    }

    static Fraction formFraction(String str) {
        String cm[] = str.split("/");
        if (cm.length == 1) {
            return new Fraction(new BigInteger(cm[0]));
        } else {
            return new Fraction(new BigInteger(cm[0]), new BigInteger(cm[1]));
        }
    }

    static class Answer {
        Point[] source;
        int[][] facets;
        Point[] destination;

        public Answer(Point[] source, int[][] facets, Point[] destination) {
            this.source = source;
            this.facets = facets;
            this.destination = destination;
        }
    }

    static Answer answer(Point p[]) {//silhouette
        int N = p.length;
        Fraction xmin = null, ymin = null, xmax = null, ymax = null;

        for (int i = 0; i < N; i++) {
            Fraction x = p[i].x;
            Fraction y = p[i].y;

            if(i == 0) {
                xmin = x;
                xmax = x;
                ymin = y;
                ymax = y;
            } else {
                xmin = xmin.min(x);
                xmax = xmax.max(x);
                ymin = ymin.min(y);
                ymax = ymax.max(y);
            }
        }

        Fraction W = xmax.sub(xmin);
        Fraction H = ymax.sub(ymin);
        Point base = new Point(xmin, ymin);


        Point[] source = getSource(W, H);
        int[][] facets = getFacets(W, H);
        Point[] destination = adder(getDestination(W, H), base);

        return new Answer(source, facets, destination);
    }

    static void outputAnswer(PrintWriter pw, Answer ans) {
        Point[] source = ans.source;
        int[][] facets = ans.facets;
        Point[] destination = ans.destination;

        int N = source.length;
        if (source.length != destination.length) {
            System.err.println("|source| != |destination|");
            System.exit(-1);
        }
        pw.println(N);
        outputPoints(pw, source);
        pw.println(facets.length);
        for (int i = 0; i < facets.length; i++) {
            StringBuilder sb = new StringBuilder();
            sb.append(facets[i].length);
            for (int j = 0; j < facets[i].length; j++) {
                sb.append(" " + facets[i][j]);
            }
            pw.println(sb);
        }
        outputPoints(pw, destination);
        pw.flush();
    }

    static void outputPoints(PrintWriter pw, Point[] ps) {
        for(Point p: ps) {
            pw.println(p);
        }
    }

    static Point[] adder(Point ps[], Point p) {
        Point[] res = new Point[ps.length];
        for (int i = 0; i < ps.length; i++) {
            res[i] = ps[i].add(p);
        }
        return res;
    }

    static Point[] getSource(Fraction W, Fraction H) {
        if (W.compareTo(Fraction.ONE) >= 0 && H.compareTo(Fraction.ONE) >= 0) {
            return new Point[] {
                    Point.p[0][0],
                    Point.p[1][0],
                    Point.p[1][1],
                    Point.p[0][1]
            };
        } else if (W.compareTo(Fraction.ONE) >= 0) {//Use H only
            return new Point[] {
                    Point.p[0][0],
                    Point.p[1][0],
                    new Point(Fraction.ONE, H),
                    Point.p[1][1],
                    Point.p[0][1],
                    new Point(Fraction.ZERO, H)
            };
        } else if (H.compareTo(Fraction.ONE) >= 0) {//Use W only
            return new Point[] {
                    Point.p[0][0],
                    new Point(W, Fraction.ZERO),
                    Point.p[1][0],
                    Point.p[1][1],
                    new Point(W, Fraction.ONE),
                    Point.p[0][1],
            };
        }

        return new Point[]{
                Point.p[0][0],
                new Point(W, Fraction.ZERO),
                Point.p[1][0],
                new Point(Fraction.ONE, H),
                Point.p[1][1],
                new Point(W, Fraction.ONE),
                Point.p[0][1],
                new Point(Fraction.ZERO, H),
                new Point(W, H)
        };
    }

    static int[][] getFacets(Fraction W, Fraction H) {
        if (W.compareTo(Fraction.ONE) >= 0 && H.compareTo(Fraction.ONE) >= 0) {
            return new int[][] {
                    {0, 1, 2, 3}
            };
        } else if (W.compareTo(Fraction.ONE) >= 0) {//Use H only
            return new int[][] {
                    {0, 1, 2, 5},
                    {5, 2, 3, 4},
            };
        } else if (H.compareTo(Fraction.ONE) >= 0) {//Use W only
            return new int[][] {
                    {0, 1, 4, 5},
                    {1, 2, 3, 4}
            };
        }

        return new int[][] {
            {0, 1, 8, 7},
            {1, 2, 3, 8},
            {8, 3, 4, 5},
            {7, 8, 5, 6}
        };
    }

    static Point[] getDestination(Fraction W, Fraction H) {
        Fraction W2sub1 = W.add(W).sub(Fraction.ONE);
        Fraction H2sub1 = H.add(H).sub(Fraction.ONE);

        if (W.compareTo(Fraction.ONE) >= 0 && H.compareTo(Fraction.ONE) >= 0) {
            return new Point[] {
                    Point.p[0][0],
                    Point.p[1][0],
                    Point.p[1][1],
                    Point.p[0][1]
            };
        } else if (W.compareTo(Fraction.ONE) >= 0) {//Use H only
            return new Point[] {
                    Point.p[0][0],
                    Point.p[1][0],
                    new Point(Fraction.ONE, H),
                    new Point(Fraction.ONE, H2sub1),
                    new Point(Fraction.ZERO, H2sub1),
                    new Point(Fraction.ZERO, H)
            };
        } else if (H.compareTo(Fraction.ONE) >= 0) {//Use W only
            return new Point[] {
                    Point.p[0][0],
                    new Point(W, Fraction.ZERO),
                    new Point(W2sub1, Fraction.ZERO),
                    new Point(W2sub1, Fraction.ONE),
                    new Point(W, Fraction.ONE),
                    Point.p[0][1],
            };
        }

        return new Point[]{
                Point.p[0][0],
                new Point(W, Fraction.ZERO),
                new Point(W2sub1, Fraction.ZERO),
                new Point(W2sub1, H),
                new Point(W2sub1, H2sub1),
                new Point(W, H2sub1),
                new Point(Fraction.ZERO, H2sub1),
                new Point(Fraction.ZERO, H),
                new Point(W, H)
        };
    }
}
