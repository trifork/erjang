package erjang.beam;


public interface BeamFunction {

	boolean isExported();

	String getModuleName();

	String getName();

	int getArity();

	int getXregCount();

	int getYregCount();

	int getFregCount();

	int getEntryLabel();

	BeamCodeBlock[] getCodeBlocks();

}
