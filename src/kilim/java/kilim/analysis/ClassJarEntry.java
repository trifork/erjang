package kilim.analysis;

import java.util.jar.JarEntry;

public class ClassJarEntry extends JarEntry {

	public ClassJarEntry(ClassInfo info) {
		super(info.className + ".class");
	}

	
}
