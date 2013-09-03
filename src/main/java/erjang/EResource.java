package erjang;

public class EResource extends EPseudoTerm {

	long handle;
	long type;
	long cleanup_callback_fn;
	
	@Override
	public int hashCode() {
		return (int)(handle >> 2);
	}

	@Override
	protected void finalize() throws Throwable {
		jni_finalize(handle);
	}
	
	private static native void jni_finalize(long handle);
}
