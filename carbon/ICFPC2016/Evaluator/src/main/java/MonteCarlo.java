import com.google.common.base.Predicates;
import com.google.common.collect.ImmutableList;
import org.apache.commons.math3.random.BitsStreamGenerator;
import org.apache.commons.math3.random.MersenneTwister;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class MonteCarlo {

    private static final int ITERATION_COUNT = 100000;
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

    public double run() {
        //System.out.printf("Boundary box: %s\n", boundaryBox);
        int insideCount = 0;
        for (int i = 0; i < ITERATION_COUNT; i++) {
            final Vector sample = boundaryBox.generateInnnerPoint(generator);
            final ImmutableList<Boolean> included = ImmutableList.copyOf(polygonList.stream().map(p -> p.inside(sample)).collect(Collectors.toList()));
            //System.out.printf("Sample: %s, Included: %s\n", sample, included);
            if (inclusion.apply(included.stream())) {
                insideCount++;
            }
        }
        return (double) insideCount / (double) ITERATION_COUNT * boundaryBox.getArea();
    }

}
