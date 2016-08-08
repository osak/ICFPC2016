import java.math.BigInteger;

/**
 * Created by keitak on 8/5/16.
 */
public class Fraction {
    static BigInteger MINUS_ONE = BigInteger.valueOf(-1);
    public static Fraction ZERO = new Fraction(BigInteger.ZERO, BigInteger.ONE);
    public static Fraction ONE = new Fraction(BigInteger.ONE, BigInteger.ONE);

    BigInteger child;
    BigInteger mother;

    public Fraction(BigInteger child, BigInteger mother) {
        BigInteger gcd = child.gcd(mother);
        child = child.divide(gcd);
        mother = mother.divide(gcd);

        if (child.equals(BigInteger.ZERO)) {
            this.child = BigInteger.ZERO;
            this.mother = BigInteger.ONE;
        } else if (mother.compareTo(BigInteger.ZERO) < 0) {
            this.child = child.multiply(MINUS_ONE);
            this.mother = mother.multiply(MINUS_ONE);
        } else {
            this.child = child;
            this.mother = mother;
        }
    }

    public Fraction(BigInteger child) {
        this.child = child;
        this.mother = BigInteger.ONE;
    }

    Fraction add(Fraction f) {
        BigInteger nextMom = mother.multiply(f.mother);
        BigInteger nextChi = child.multiply(f.mother).add(mother.multiply(f.child));
        return new Fraction(nextChi, nextMom);
    }

    Fraction mul(Fraction f) {
        return new Fraction(child.multiply(f.child), mother.multiply(f.mother));
    }

    Fraction sub(Fraction f) {
        return add(f.minus());
    }

    Fraction minus() {
        return new Fraction(child.multiply(MINUS_ONE), mother);
    }

    Fraction min(Fraction f) {
        if (compareTo(f) < 0) {
            return this;
        } else {
            return f;
        }
    }

    Fraction max(Fraction f) {
        if (compareTo(f) > 0) {
            return this;
        } else {
            return f;
        }
    }

    int compareTo(Fraction f) {
        return sub(f).child.compareTo(BigInteger.ZERO);
    }

    static public BigInteger lcm(BigInteger a, BigInteger b) {
        return a.multiply(b).divide(a.gcd(b));
    }

    public String toString() {
        if (mother.equals(BigInteger.ONE)) {
            return child.toString();
        } else {
            return child + "/" + mother;
        }
    }

    @Override
    public int hashCode() {
        return child.hashCode() * 31 + mother.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        Fraction f = (Fraction)obj;
        return child.equals(f.child) && mother.equals(f.mother);
    }
}
