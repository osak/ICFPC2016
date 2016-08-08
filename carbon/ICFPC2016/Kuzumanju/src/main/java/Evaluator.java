import com.google.common.base.Predicates;
import com.google.common.base.Stopwatch;
import com.google.common.collect.ImmutableList;
import org.apache.commons.math3.random.BitsStreamGenerator;
import org.apache.commons.math3.random.MersenneTwister;

import java.io.File;
import java.util.Scanner;
import java.util.concurrent.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Evaluator {

    public static void main(final String[] args) throws Exception {
        final Stopwatch stopwatch = Stopwatch.createStarted();
        System.out.println(System.getProperty("user.dir"));
        final Scanner scanner = new Scanner(new File(args[0]));
        final Evaluator evaluator = new Evaluator();
        final ForkJoinPool forkJoinPool = new ForkJoinPool();
        final Silhouette silhouette = Silhouette.parseSilhouette(scanner);
        final Destination destination = Destination.parseDestination(scanner, evaluator.parseSourcePositions(scanner).size());
        final Rectangle silhouetteBoundaryBox = silhouette.getBoundaryBox();
        final Rectangle destinationBoundaryBox = destination.getBoundaryBox();
        final Rectangle intersectionBoundaryBox = silhouetteBoundaryBox.intersection(destinationBoundaryBox);
        final Rectangle spanBoundaryBox = silhouetteBoundaryBox.span(destinationBoundaryBox);

        final Future<Double> intersectionAreaFuture = forkJoinPool.submit(
                new MonteCarlo(
                        intersectionBoundaryBox,
                        ImmutableList.of(silhouette, destination),
                        bs -> bs.allMatch(b -> b)
                ));

        final Future<Double> spanAreaFuture = forkJoinPool.submit(
                new MonteCarlo(
                        spanBoundaryBox,
                        ImmutableList.of(silhouette, destination),
                        bs -> bs.anyMatch(b -> b)
                ));
        final double resemblance = intersectionAreaFuture.get() / spanAreaFuture.get();
        System.out.printf("Calculation time (ms): %s\n", stopwatch.elapsed(TimeUnit.MILLISECONDS));
        System.out.printf("Resemblance: %s\n", resemblance);
        forkJoinPool.shutdown();
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
