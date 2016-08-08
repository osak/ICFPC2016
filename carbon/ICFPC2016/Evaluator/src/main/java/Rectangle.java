import com.google.common.collect.Range;
import org.apache.commons.math3.random.BitsStreamGenerator;
import org.immutables.value.Value;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Value.Immutable
public abstract class Rectangle {

    @Value.Parameter public abstract Range<Double> getXRange();
    @Value.Parameter public abstract Range<Double> getYRange();

    public Rectangle intersection(final Rectangle that) {
        final Range<Double> xRange = this.getXRange().intersection(that.getXRange());
        final Range<Double> yRange = this.getYRange().intersection(that.getYRange());
        return ImmutableRectangle.of(xRange, yRange);
    }

    public Rectangle span(final Rectangle that) {
        final Range<Double> xRange = this.getXRange().span(that.getXRange());
        final Range<Double> yRange = this.getYRange().span(that.getYRange());
        return ImmutableRectangle.of(xRange, yRange);
    }

    public Vector generateInnnerPoint(final BitsStreamGenerator generator) {
        return Vector.of(dividingPoint(generator.nextDouble(), getXRange()),
                dividingPoint(generator.nextDouble(), getYRange()));
    }

    private double dividingPoint(final double sample, final Range<Double> range) {
        final double length = range.upperEndpoint() - range.lowerEndpoint();
        return range.lowerEndpoint() + length * sample;
    }

    public double getArea() {
        final double width = getXRange().upperEndpoint() - getXRange().lowerEndpoint();
        final double height = getYRange().upperEndpoint() - getYRange().lowerEndpoint();
        return width * height;
    }

    public static Rectangle calculateBoundaryBox(final Collection<Vector> positions) {
        final Range<Double> xRange = Range.encloseAll(
                positions.stream()
                        .map(Vector::getX)
                        .collect(Collectors.toList()));
        final Range<Double> yRange = Range.encloseAll(
                positions.stream()
                        .map(Vector::getY)
                        .collect(Collectors.toList()));
        return ImmutableRectangle.of(xRange, yRange);
    }

}
