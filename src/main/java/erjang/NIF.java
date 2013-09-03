package erjang;

public class NIF {

	/** pointer to struct jnif_module */
	private long module_ptr;
	
	/** call NIF function number <code>index</code>, with <code>args</code>. */
	private native EObject jni_invoke(long module_ptr, int index, EObject[] args);
	
	/** load NIF */
	private native long jni_load(String path, EObject info);
}
