package erjang;

import java.io.File;

/**
 * Test case for simple property tests not needing the erlang node access
 *
 @author Pavlo Baron <pb@pbit.org>
 */
public class PropertyTestCase extends AbstractTestCaseWithoutErlangNodeAccess {

    public PropertyTestCase(String name) {
        super(name);
    }

	/**
	 * @param file
	 */
	public PropertyTestCase(File file) {
		super(file);
	}

    /* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return "Property test " + file.getName() + " (without node access)";
	}
}
