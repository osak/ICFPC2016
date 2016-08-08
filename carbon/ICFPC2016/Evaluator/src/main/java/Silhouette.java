import org.immutables.value.Value;

import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

@Value.Immutable
public abstract class Silhouette implements Polygons {

    public abstract List<Polygon> getPolygons();

    @Value.Derived
    public Rectangle getBoundaryBox() {
        return Rectangle.calculateBoundaryBox(
                getPolygons().stream()
                        .flatMap(p -> p.getVertices().stream())
                        .collect(Collectors.toList())
        );
    }

    @Override
    public boolean inside(final Vector point) {
        final boolean insideOfFirstPolygon = getPolygons().get(0).inside(point);
        final boolean insideOfHoles = getPolygons().stream()
                .skip(1)
                .anyMatch(p -> p.inside(point));
        return insideOfFirstPolygon && !insideOfHoles;
    }

    public static Silhouette parseSilhouette(final Scanner scanner) {
        final int silhouettePolygonCount = Integer.parseInt(scanner.nextLine());
        final ImmutableSilhouette.Builder builder = ImmutableSilhouette.builder();
        for (int i = 0; i < silhouettePolygonCount; i++) {
            builder.addPolygons(Polygon.parse(scanner));
        }
        return builder.build();
    }

}
