package erjang.beam;

public interface BeamCodeBlock {

	boolean isDeadCode();

	int getLabel();

	BeamInstruction[] getInstructions();

}
