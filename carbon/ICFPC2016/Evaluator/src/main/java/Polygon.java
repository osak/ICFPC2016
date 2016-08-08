import com.google.common.collect.ImmutableList;
import com.google.common.collect.Range;
import org.immutables.value.Value;

import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Value.Immutable
public abstract class Polygon {

    public abstract List<Vector> getVertices();

    @Value.Derived
    public Rectangle getBoundaryBox() {
        return Rectangle.calculateBoundaryBox(getVertices());
    }

    @Value.Derived
    public Rectangle getRightHorizontalBox() {
        final double right = getBoundaryBox().getXRange().upperEndpoint();
        final Range<Double> yRange = getBoundaryBox().getYRange();
        final Range<Double> xRange = Range.closed(right + 1, right + 2);
        return ImmutableRectangle.of(xRange, Range.closed(yRange.lowerEndpoint() - 100000, yRange.upperEndpoint() + 100000));
    }

    @Value.Derived
    public LineSegment getRightHorizontalLine() {
        final double x = getBoundaryBox().getXRange().upperEndpoint() + 1.;
        return ImmutableLineSegment.of(
                Vector.of(x, getBoundaryBox().getYRange().lowerEndpoint()),
                Vector.of(x, getBoundaryBox().getYRange().lowerEndpoint() + 10000.)
        );
    }

    @Value.Derived
    public List<LineSegment> getEdges() {
        final ImmutableList.Builder<LineSegment> builder = ImmutableList.builder();
        final Vector first = getVertices().get(0);
        final List<Vector> rotatedVertices = Stream.concat(getVertices().stream(), Stream.of(first)).collect(Collectors.toList());
        for (int i = 0; i < rotatedVertices.size() - 1; i++) {
            builder.add(ImmutableLineSegment.of(rotatedVertices.get(i), rotatedVertices.get(i + 1)));
        }
        return builder.build();
    }

    public boolean inside(final Vector point) {
        for (Vector endPoint = getRightHorizontalLine().getBegin();
             true;
             endPoint = generateEndPoint(endPoint)) {
            try {
                final ImmutableLineSegment lineSegment = ImmutableLineSegment.of(point, endPoint);
                final int crossCount = getEdges().stream()
                        .collect(Collectors.summingInt(e -> {
                            switch (lineSegment.crosses(e)) {
                                case CROSSES:
                                    return 1;
                                case DISJOINT:
                                    return 0;
                                case THIS_TOUCHES:
                                    return 1;
                                case THAT_TOUCHES:
                                    throw new IllegalStateException();
                                case JOINT:
                                    return 1;
                            }
                            return 0;
                        }));
                return crossCount % 2 != 0;
            } catch (final IllegalStateException ignore) {

            }
        }
    }

    private Vector generateEndPoint(final Vector lastEndPoint) {
        return Vector.of(lastEndPoint.getX(), lastEndPoint.getY() + 1);
    }

    public static Polygon parse(final Scanner scanner) {
        final int vertexCount = Integer.parseInt(scanner.nextLine());
        final ImmutablePolygon.Builder builder = ImmutablePolygon.builder();
        for (int i = 0; i < vertexCount; i++) {
            builder.addVertices(Vector.parse(scanner.nextLine()));
        }
        return builder.build();
    }

}
