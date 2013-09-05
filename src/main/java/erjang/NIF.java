package erjang;

import java.util.HashMap;

import kilim.Pausable;

public class NIF {

    static {
        System.loadLibrary("jnif");
    }

    /** pointer to struct jnif_module */
    private long module_ptr;
    private String module_name;
    private HashMap<FunID, Integer> nif_funs = new HashMap<FunID, Integer>();

    private NIF(final long ptr) {
        module_ptr = ptr;
        init();
    }
    
    void init() {
    	final long ptr = module_ptr;
        module_name = jni_module_name(ptr);
        int count = jni_fun_count(ptr);
        
        for (int i = 0; i < count; i++) {
            final String name = jni_fun_name(ptr, i);
            final int arity   = jni_fun_arity(ptr, i);
            final FunID fid = new FunID(module_name, name, arity);
            nif_funs.put(fid, i);

            // System.err.println("loaded " + fid + " => " + i );
            
            final int idx = i;
            EFunHandler nif_handler = new EFunHandler() {
            	@Override
            	public EObject invoke(EProc proc, EObject[] args)
            			throws Pausable {
            		return NIF.this.jni_invoke(proc, ptr, idx, args);
            	}

            	@Override
				public String toString() {
					return "#NIF<" + fid + ">";
				}

            };

            try {
				EModuleManager.bind_nif(fid, nif_handler);
			} catch (Exception e) {
				throw new RuntimeException("unable to bind "+fid, e);
			}
        }
    }

    /** call NIF function number <code>index</code>, with <code>args</code>. */
    private native EObject jni_invoke(EProc proc, long module_ptr, int index, EObject[] args);

    /** load NIF */
    private static native long jni_load(String path, EObject info);

    private static native String jni_module_name(long ptr);
    private static native int jni_fun_count(long ptr);
    private static native String jni_fun_name(long ptr, int idx);
    private static native int jni_fun_arity(long ptr, int idx);


    public static NIF load(String path, EObject info)
    {
        long ptr = jni_load(path, info);
        if (ptr != 0L) {
            return new NIF(ptr);
        }

        return null;
    }

    public static NIF load(String path)
    {
        return load(path, ERT.am_undefined);
    }
}
