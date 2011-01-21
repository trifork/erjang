/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang;

import java.io.File;

/**
 * Generator for different Erjang unit test cases (unit test suites)
 *
 * @author Pavlo Baron <pb@pbit.org>
 */
public abstract class TestClassGenerator {

	protected static String generateClassSource(Class<? extends AbstractErjangTestCase> clazz, File file) {
        StringBuffer s = new StringBuffer();
        String name = classNameFor(file);
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

    public static String classNameFor(File file) {
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