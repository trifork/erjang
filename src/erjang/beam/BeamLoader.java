package erjang.beam;

import java.io.File;
import java.io.IOException;

public abstract class BeamLoader {

	abstract BeamFile load(File file) throws IOException;
	
}
