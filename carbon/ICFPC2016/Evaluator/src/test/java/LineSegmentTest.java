import com.google.common.collect.ImmutableList;
import org.immutables.value.Value;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

@RunWith(Parameterized.class)
public class LineSegmentTest {

    private final LineSegment thisLine;
    private final LineSegment thatLine;
    private final LineSegment.CrossPattern answer;

    public LineSegmentTest(LineSegment thisLine, LineSegment thatLine, LineSegment.CrossPattern answer) {
        this.thisLine = thisLine;
        this.thatLine = thatLine;
        this.answer = answer;
    }

    @Parameterized.Parameters
    public static Collection<Object[]> parameters() {
        return ImmutableList.of(
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(-0.5, 0.5), Vector.of(-0.25, 0.5)),
                        LineSegment.CrossPattern.DISJOINT
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(-0.5, 0.5), Vector.of(0.5, 0.5)),
                        LineSegment.CrossPattern.CROSSES
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(-0.5, 1), Vector.of(-0.25, 1)),
                        LineSegment.CrossPattern.DISJOINT
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(-0.5, 1), Vector.of(0.5, 1)),
                        LineSegment.CrossPattern.THIS_TOUCHES
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(-1, 0), Vector.of(-1, 1)),
                        LineSegment.CrossPattern.DISJOINT
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(0, 0.2), Vector.of(0, 0.5)),
                        LineSegment.CrossPattern.JOINT
                },
                new Object[] {
                        ImmutableLineSegment.of(Vector.of(0, 0), Vector.of(0, 1)),
                        ImmutableLineSegment.of(Vector.of(0, 0.5), Vector.of(1, 0.5)),
                        LineSegment.CrossPattern.THAT_TOUCHES
                }
        );
    }

    @Test
    public void test() {
        assertEquals(answer, thisLine.crosses(thatLine));
    }

}
