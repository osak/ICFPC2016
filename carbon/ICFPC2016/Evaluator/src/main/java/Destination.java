import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import org.immutables.value.Value;

import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

@Value.Immutable
public abstract class Destination implements Polygons {

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
        return getPolygons().stream().anyMatch(p -> p.inside(point));
    }

    public static Destination parseDestination(
            final Scanner scanner,
            final int positionsCount
    ) {
        final ImmutableList<Facet> facets = parseFacets(scanner);
        final ImmutableMap<Integer, Vector> destinationPositions = parseDestinationPositions(scanner, positionsCount);
        return ImmutableDestination.builder().addAllPolygons(
                facets.stream()
                        .map(f -> (Polygon) ImmutablePolygon.builder().addAllVertices(
                                f.getVertices().stream()
                                        .map(destinationPositions::get)
                                        .collect(Collectors.toList())
                        ).build())
                        .collect(Collectors.toList())
        ).build();
    }

    private static ImmutableMap<Integer, Vector> parseDestinationPositions(final Scanner scanner, final int positionsCount) {
        final ImmutableMap.Builder<Integer, Vector> builder = ImmutableMap.builder();
        for (int i = 0; i < positionsCount; i++) {
            builder.put(i, Vector.parse(scanner.nextLine()));
        }
        return builder.build();
    }

    private static ImmutableList<Facet> parseFacets(final Scanner scanner) {
        final int facetCount = Integer.parseInt(scanner.nextLine());
        final ImmutableList.Builder<Facet> facetBuilder = ImmutableList.builder();
        for (int i = 0; i < facetCount; i++) {
            facetBuilder.add(Facet.parse(scanner.nextLine()));
        }
        return facetBuilder.build();
    }

    @Value.Immutable
    public static abstract class Facet {
        public abstract List<Integer> getVertices();
        public static Facet parse(final String line) {
            final Scanner scanner = new Scanner(line);
            final int verticesCount = scanner.nextInt();
            final ImmutableFacet.Builder builder = ImmutableFacet.builder();
            for (int i = 0; i < verticesCount; i++) {
                builder.addVertices(scanner.nextInt());
            }
            return builder.build();
        }
    }

}
