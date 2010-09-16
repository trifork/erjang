package erjang.epmd;

class Functions {

	private static int debugLevel;

	public static int toInt(String intString, int defaultValue) {
		try {
			return Integer.parseInt(intString);
		} catch (NumberFormatException e) {
			return defaultValue;
		}
	}

	public static void setDebugLevel(int level) {
		debugLevel = level;
	}

	public static void dout(int level, String string) {
		if (debugLevel >= level) {
			System.err.println(string);
		}
	}

	public static void fail(Exception e, String string) {
		System.err.println(string);
		e.printStackTrace(System.err);
		System.exit(1);
	}

}
