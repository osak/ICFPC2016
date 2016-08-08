import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import org.apache.commons.math3.random.BitsStreamGenerator;
import org.apache.commons.math3.random.MersenneTwister;

import java.io.File;
import java.util.Scanner;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Evaluator {

    public static void main(final String[] args) throws Exception {
        System.out.println(System.getProperty("user.dir"));
        final Scanner scanner = new Scanner(new File("Evaluator/etc/example_input.txt"));
        final Evaluator evaluator = new Evaluator();
        final Silhouette silhouette = Silhouette.parseSilhouette(scanner);
        final Destination destination = Destination.parseDestination(scanner, evaluator.parseSourcePositions(scanner).size());
        final Rectangle silhouetteBoundaryBox = silhouette.getBoundaryBox();
        final Rectangle destinationBoundaryBox = destination.getBoundaryBox();
        final Rectangle intersectionBoundaryBox = silhouetteBoundaryBox.intersection(destinationBoundaryBox);
        final Rectangle spanBoundaryBox = silhouetteBoundaryBox.span(destinationBoundaryBox);

        final double intersectionArea =
                new MonteCarlo(
                        intersectionBoundaryBox,
                        ImmutableList.of(silhouette, destination),
                        bs -> bs.allMatch(b -> b)
                ).run();
        final double spanArea =
                new MonteCarlo(
                        spanBoundaryBox,
                        ImmutableList.of(silhouette, destination),
                        bs -> bs.anyMatch(b -> b)
                ).run();
        System.out.printf("Resemblance: %s\n", intersectionArea / spanArea);
    }

    public ImmutableList<Vector> parseSourcePositions(final Scanner scanner) {
        final int sourcePositionCount = Integer.parseInt(scanner.nextLine());
        final ImmutableList.Builder<Vector> sourcePositionsBuilder = ImmutableList.builder();
        for (int i = 0; i < sourcePositionCount; i++) {
            sourcePositionsBuilder.add(Vector.parse(scanner.nextLine()));
        }
        return sourcePositionsBuilder.build();
    }

}
