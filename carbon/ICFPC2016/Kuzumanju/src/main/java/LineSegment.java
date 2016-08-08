import org.immutables.value.Value;

@Value.Immutable
public abstract class LineSegment {

    @Value.Parameter public abstract Vector getBegin();
    @Value.Parameter public abstract Vector getEnd();

    public int counterclockwise(final Vector target) {
        final double cross = getEnd().subtract(getBegin()).cross(target.subtract(getBegin()));
        return Double.compare(cross, 0);
    }

    public CrossPattern crosses(final LineSegment that) {
        final int thatPosition = this.counterclockwise(that.getBegin()) * this.counterclockwise(that.getEnd());
        final int thisPosition = that.counterclockwise(this.getBegin()) * that.counterclockwise(this.getEnd());
        if (thisPosition == 0 && thatPosition == 0) {
            return CrossPattern.JOINT;
        } else if (thisPosition == 0 && thatPosition < 0) {
            return CrossPattern.THIS_TOUCHES;
        } else if (thatPosition == 0 && thisPosition < 0) {
            return CrossPattern.THAT_TOUCHES;
        } else if (thisPosition < 0 && thatPosition < 0){
            return CrossPattern.CROSSES;
        } else {
            return CrossPattern.DISJOINT;
        }
    }

    public enum CrossPattern {
        CROSSES,
        THAT_TOUCHES,
        THIS_TOUCHES,
        JOINT,
        DISJOINT
    }

}
