import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import org.apache.commons.math3.random.BitsStreamGenerator;
import org.apache.commons.math3.random.MersenneTwister;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MonteCarlo extends RecursiveTask<Double> {

    private static final int ITERATION_COUNT = 1000000;
    private static final int BATCH_SIZE = 100;
    private final BitsStreamGenerator generator = new MersenneTwister(2);
    private final Rectangle boundaryBox;
    private final List<Polygons> polygonList;
    private final Function<Stream<Boolean>, Boolean> inclusion;

    public MonteCarlo(
            final Rectangle boundaryBox,
            final List<Polygons> polygonList,
            final Function<Stream<Boolean>, Boolean> inclusion
    ) {
        this.boundaryBox = boundaryBox;
        this.polygonList = polygonList;
        this.inclusion = inclusion;
    }

    @Override
    protected Double compute() {
        //System.out.printf("Boundary box: %s\n", boundaryBox);
        final ImmutableList.Builder<RecursiveTask<Integer>> builder = ImmutableList.builder();
        for (int i = 0; i < ITERATION_COUNT; i += BATCH_SIZE) {
            final RecursiveTask<Integer> task = new RecursiveTask<Integer>() {
                @Override
                protected Integer compute() {
                    int insideCount = 0;
                    for (int j = 0; j < BATCH_SIZE; j++) {
                        final Vector sample = boundaryBox.generateInnnerPoint(generator);
                        final ImmutableList<Boolean> included = ImmutableList.copyOf(polygonList.stream().map(p -> p.inside(sample)).collect(Collectors.toList()));
                        //System.out.printf("Sample: %s, Included: %s\n", sample, included);
                        if (inclusion.apply(included.stream())) {
                            insideCount++;
                        }
                    }
                    return insideCount;
                }
            };
            task.fork();
            builder.add(task);
        }
        final Integer insideCount = builder.build().stream()
                .collect(Collectors.summingInt(f -> {
                    try {
                        return f.get();
                    } catch (final Exception e) {
                        return 0;
                    }
                }));
        return (double) insideCount / (double) ITERATION_COUNT * boundaryBox.getArea();
    }

}
