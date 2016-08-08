import com.google.common.collect.ImmutableList;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Collection;

import static org.junit.Assert.assertEquals;

@RunWith(Parameterized.class)
public class PolygonsTest {

    private final Polygons polygons;
    private final Vector point;
    private final boolean answer;

    public PolygonsTest(Polygons polygons, Vector point, boolean answer) {
        this.polygons = polygons;
        this.point = point;
        this.answer = answer;
    }

    private static final ImmutableList<Polygons> POLYGONS = ImmutableList.of(
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(1, 1),
                            Vector.of(3, 1),
                            Vector.of(3, 4),
                            Vector.of(1, 4)
                    ).build()
            ).build(),
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(2, 4),
                            Vector.of(3, 5),
                            Vector.of(5, 3),
                            Vector.of(4, 2)
                    ).build()
            ).build(),
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(1, 3),
                            Vector.of(2, 6),
                            Vector.of(5, 8),
                            Vector.of(8, 6),
                            Vector.of(10, 3),
                            Vector.of(9, -1),
                            Vector.of(5, -2),
                            Vector.of(2, 0)
                    ).build(),
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(3, 5),
                            Vector.of(7, 5),
                            Vector.of(7, 1),
                            Vector.of(3, 1)
                    ).build()
            ).build(),
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(7, 16),
                            Vector.of(15, 13),
                            Vector.of(16, 9),
                            Vector.of(7, 0),
                            Vector.of(3, 1),
                            Vector.of(0, 9),
                            Vector.of(2, 9),
                            Vector.of(4, 3),
                            Vector.of(7, 2),
                            Vector.of(10, 5),
                            Vector.of(13, 8),
                            Vector.of(14, 9),
                            Vector.of(13, 12),
                            Vector.of(7, 14)
                    ).build(),
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(7, 12),
                            Vector.of(9, 10),
                            Vector.of(8, 9),
                            Vector.of(7, 10),
                            Vector.of(6, 9),
                            Vector.of(7, 8),
                            Vector.of(6, 7),
                            Vector.of(4, 9)
                    ).build()
            ).build(),
            //Problem 18
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(0, 0.5),
                            Vector.of(0, 1),
                            Vector.of(13./22., 1),
                            Vector.of(3./5., 21./20.),
                            Vector.of(21./25., 28./25.),
                            Vector.of(7./8., 1.),
                            Vector.of(1, 1),
                            Vector.of(1, 3./4.),
                            Vector.of(1./2., 1./2.)
                    ).build()
            ).build(),
            // kawatea answer to problem 18
            ImmutablePolygons.builder().addPolygons(
                    ImmutablePolygon.builder().addVertices(
                            Vector.of(0., 1./2.),
                            Vector.of(0., 1.),
                            Vector.of(13./22., 1),
                            Vector.of(21./25., 28./25.),
                            Vector.of(1, 1),
                            Vector.of(1, 3./4.),
                            Vector.of(1./2., 1./2.)
                    ).build()
            ).build()
    );

    @Parameterized.Parameters
    public static Collection<Object[]> data() {

        return ImmutableList.of(
                new Object[] {
                        POLYGONS.get(0),
                        Vector.of(2, 2),
                        true
                },
                new Object[] {
                        POLYGONS.get(0),
                        Vector.of(0, 2),
                        false
                },
                new Object[] {
                        POLYGONS.get(1),
                        Vector.of(2, 2),
                        false
                },
                new Object[] {
                        POLYGONS.get(2),
                        Vector.of(5, 3),
                        false
                },
                new Object[] {
                        POLYGONS.get(2),
                        Vector.of(8, 3),
                        true
                },
                new Object[] {
                        POLYGONS.get(3),
                        Vector.of(7, 9),
                        false
                },
                new Object[] {
                        POLYGONS.get(3),
                        Vector.of(11, 5),
                        true
                },
                new Object[] {
                        POLYGONS.get(4),
                        Vector.of(0.5496624849398524, 0.7698998856625623),
                        true
                },
                new Object[] {
                        POLYGONS.get(5),
                        Vector.of(0.5496624849398524, 0.7698998856625623),
                        true
                }
        );
    }

    @Test
    public void test() {
        assertEquals(answer, polygons.inside(point));
    }

}
