package erjang;

import junit.framework.Assert;
import junit.framework.AssertionFailedError;
import junit.framework.TestResult;

import java.io.File;

/**
 *
 * Base class for all test cases which don't need the erlang node access
 *
 * @author Pavlo Baron <pb@pbit.org>
 */
public abstract class AbstractTestCaseWithoutErlangNodeAccess extends AbstractErjangTestCase {

    static final String RUN_WRAPPER_HOME = "src/test/erl";

    public AbstractTestCaseWithoutErlangNodeAccess(String name) {
        super(name);
    }

	/**
	 * @param file
	 */
	public AbstractTestCaseWithoutErlangNodeAccess(File file) {
		super(file);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public abstract String toString();

	/* (non-Javadoc)
	 * @see junit.framework.Test#countTestCases()
	 */
	@Override
	public int countTestCases() {
		return 1;
	}

	/* (non-Javadoc)
	 * @see junit.framework.Test#run(junit.framework.TestResult)
	 */
	@Override
	public void run(TestResult result) {
		result.startTest(this);
		try {
			TestUtil.erl_compile(RUN_WRAPPER_HOME + File.separator +"run_wrapper.erl");
			TestUtil.erl_compile(file.getAbsolutePath());
			EObject expected = do_run(file, TestUtil.ERL_PRG);
            EObject actual = do_run(file, TestUtil.get_ej());

			Assert.assertEquals(expected, actual);
		} catch (AssertionFailedError e) {
		    result.addFailure(this, e);
		} catch (Throwable e) {
			result.addError(this, e);
		}
		result.endTest(this);
	}

    protected EObject do_run(File file, String prog) throws Exception {
		String moduleName = TestUtil.trimExtension(file.getName());
		String[] cmd = new String[] {prog, "-noinput",
					     "-pa", TestUtil.TEST_BEAM_DIR,
						 "-sasl", "sasl_error_logger", "false", // Prevent SASL from polluting stdout
					     "-s", "run_wrapper", "run", "erlang", moduleName, "10",
					     "-s", "erlang", "halt"};

		byte[] bin = TestUtil.execGetBinaryOutput(cmd);

        return processOutput(bin);
	}
}
