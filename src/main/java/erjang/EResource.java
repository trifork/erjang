package erjang;

public class EResource extends EPseudoTerm {

	long handle;

	private EResource(long handle) {
		this.handle = handle;
	}
	
	private static EResource make(long handle) {
		return new EResource(handle);
	}
	
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
