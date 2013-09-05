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
	private static native String jnif_module(long handle);
	private static native String jnif_type_name(long handle);
	
	public String toString()
	{
		return "#Resource<" + jnif_module(handle) + ":'" + jnif_type_name(handle) + "':16#" + Long.toHexString(handle)+ ">";
	}

}
