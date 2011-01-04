package erjang;

import java.io.File;

import junit.framework.TestCase;

/**
 * Abstract class for Erjang JUnit test cases
 * 
 * @author Pavlo Baron <pb@pbit.org>
 *
 */
public abstract class AbstractErjangTestCase extends TestCase {

	protected File file;
	
	public AbstractErjangTestCase() {
	}
	
	public AbstractErjangTestCase(File file) {
		super(file.getName());
		this.file = file;
	}
	
	public AbstractErjangTestCase(String name) {
		super(name);
        file = new File(name);
	}
	
	public void setFile(File file) {
		this.file = file;
        this.setName(file.getName());
	}
	
	protected static String getClassContent(Class<? extends AbstractErjangTestCase> clazz, File file) {
        StringBuffer s = new StringBuffer();
        String name = getClassName(file);
        String path = getPath(file);

        s.append(getPackage());
        s.append(getImports());
        s.append(getClassStart(clazz, name));
        s.append(getConstructor(name, path));
        s.append(getSuite(name));
        s.append(getClassEnd());

        return s.toString();
    }

    protected static String getPackage() {
        return "package erjang;\n\n";
    }

    protected static String getImports() {
        StringBuffer s = new StringBuffer();

        s.append("import junit.framework.TestSuite;\n");
        s.append("import junit.framework.Test;\n\n");

        return s.toString();
    }

    public static String getClassName(File file) {
        String name = file.getName();
        name = name.replace('.', '_');
        name = name.replace('-', '_');

        return name + "_TEST";
    }

    protected static String getPath(File file) {
        String path = file.getAbsolutePath();

        //Windows backslash hack...
        path = path.replace("\\", "\\\\");

        return path;
    }

    protected static String getClassStart(Class<? extends AbstractErjangTestCase> clazz, String name) {
        return "public class " + name + " extends " + clazz.getName() + " {\n\n";
    }

    protected static String getConstructor(String name, String path) {
        StringBuffer s = new StringBuffer();

        s.append("\tpublic " + name + "() {\n");
        s.append("\t\tsuper(\"" + path + "\");\n");
        s.append("\t}\n\n");

        return s.toString();
    }

    protected static String getSuite(String name) {
        StringBuffer s = new StringBuffer();

        //backup: generate as suite: will we need to group the tests somehow?
        /*s.append("\tpublic static Test suite() {\n");
        s.append("\t\tTestSuite ts = new TestSuite();\n");
        s.append("\\ttts.addTest(new " + name + "());\n");
        s.append("\t\treturn ts;\n");
        s.append("\t}\n");*/

        s.append("\tpublic static Test suite() {\n");
        s.append("\t\treturn new " + name + "();\n");
        s.append("\t}\n");

        return s.toString();
    }

    protected static String getClassEnd() {
        return "}";
    }
}
