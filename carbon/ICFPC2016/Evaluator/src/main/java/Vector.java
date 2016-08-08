import org.apache.commons.math3.complex.Complex;
import org.immutables.value.Value;

import java.math.BigInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Value.Immutable
public abstract class Vector {

    private static final Pattern PAIR_PATTERN = Pattern.compile("(.*),(.*)");
    private static final Pattern FRACTION_PATTERN = Pattern.compile("(\\d+)/(\\d+)");

    @Value.Parameter public abstract Complex getComplex();

    public double getX() {
        return getComplex().getReal();
    }

    public double getY() {
        return getComplex().getImaginary();
    }

    public double cross(final Vector that) {
        return this.getX() * that.getY() - this.getY() * that.getX();
    }

    public Vector subtract(final Vector that) {
        return ImmutableVector.of(this.getComplex().subtract(that.getComplex()));
    }

    public static Vector parse(final String line) {
        final Matcher pairMatcher = PAIR_PATTERN.matcher(line);
        if (pairMatcher.matches()) {
            final double x = parseCoordinate(pairMatcher.group(1));
            final double y = parseCoordinate(pairMatcher.group(2));
            return Vector.of(x, y);
        } else {
            throw new IllegalStateException();
        }
    }

    private static double parseCoordinate(final String s) {
        final Matcher fractionMatcher = FRACTION_PATTERN.matcher(s);
        if (fractionMatcher.matches()) {
            return new BigInteger(fractionMatcher.group(1)).doubleValue()
                    / new BigInteger(fractionMatcher.group(2)).doubleValue();
        } else {
            return new BigInteger(s).doubleValue();
        }
    }

    public static Vector of(final double x, final double y) {
        return ImmutableVector.of(new Complex(x, y));
    }

}
