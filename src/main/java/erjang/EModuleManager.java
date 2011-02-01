/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
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
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import erjang.beam.Compiler;
import erjang.m.java.JavaObject;

import kilim.Pausable;

/**
 * 
 */
public class EModuleManager {

	static Logger log = Logger.getLogger(EModuleManager.class.getName());
	
	static private Map<EAtom, ModuleInfo> infos = new ConcurrentHashMap<EAtom, ModuleInfo>();

	static FunctionInfo undefined_function = null;

	static {
		FunID uf = new FunID("error_handler", "undefined_function", 3);
		undefined_function = get_module_info(uf.module).get_function_info(uf);
	}

	static class FunctionInfo {
		private final FunID fun;

		/**
		 * @param fun
		 */
		public FunctionInfo(FunID fun) {
			this.fun = fun;
		}

		EModule defining_module;
		EFun resolved_value;
		Collection<FunctionBinder> resolve_points = new HashSet<FunctionBinder>();
		private EFun error_handler;

	        private ClassLoader getModuleClassLoader() {
		    if (defining_module != null) {
			return defining_module.getModuleClassLoader();
		    } else {
			return new EModuleClassLoader(null);
		    }
	        }

		/**
		 * @param ref
		 * @throws IllegalAccessException
		 * @throws IllegalArgumentException
		 */
		synchronized void add_import(final FunctionBinder ref) throws Exception {
			resolve_points.add(ref);
			if (resolved_value != null) {
				// System.out.println("binding "+fun+" "+resolved_value+" -> "+ref);
				ref.bind(resolved_value);
			} else {
				EFun h = getFunErrorHandler();
				ref.bind(h);
			}
		}
		
		synchronized void unbind() throws Exception {
			for (FunctionBinder f : resolve_points) {
				f.bind(getFunErrorHandler());
			}
		}

		private EFun getFunction() {
			if (resolved_value != null) {
				return resolved_value;
			} else {
				return getFunErrorHandler();
			}
		}

		private EFun getFunErrorHandler() {
			if (error_handler != null) {
				return error_handler;
			}
			
			error_handler = makeErrorHandler();
			return error_handler;
		}

		private EFun makeErrorHandler() {
			return EFun.get_fun_with_handler(fun.arity,
					new EFunHandler() {
				
					@Override
						public String toString() {
							return "#EFunHandler<" + fun.toString() + ">";
						}
				
					public EObject invoke(EProc proc, EObject[] args)
								throws Pausable {
							
							/** Get reference to error_handler:undefined_function/3 */
							EFun uf = undefined_function.resolved_value;

							/** this is just some debugging info to help understand downstream errors */
							if (get_module_info(fun.module).is_loaded()) {
								log.log(Level.INFO, "MISSING "+fun);
							} else {
								log.log(Level.FINER, "resolving"+fun);
							}

							Class c = null;
							try {
								c = Class.forName(fun.module.getName());
							} catch (ClassNotFoundException e) {
							}

							if (c != null) {
								if (fun.function == ERT.am_new) {
									Constructor[] cons = c.getConstructors();
									return JavaObject.choose_and_invoke_constructor(proc, args, cons);
								} else {
									Method[] methods = c.getMethods();
									return JavaObject.choose_and_invoke_method(proc, null, fun.function, args, methods, true);
								}
							}
							
							if (uf == null) {
								if (!module_loaded(fun.module)) {
									try {
										EModuleLoader.find_and_load_module(fun.module.getName());
									} catch (IOException e) {
										// we don't report this separately; it ends up causing an undefined below...
									}
								}
								
								if (resolved_value != null) {
									return resolved_value.invoke(proc, args);
								}
								
								/** this is just some debugging info to help understand downstream errors */
								log.log(Level.INFO, "failed to load "+fun+" (error_handler:undefined_function/3 not found)");
								
								throw new ErlangUndefined(fun.module,
										fun.function, fun.arity);
							} else {
								ESeq arg_list = ESeq.fromArray(args);
								ESeq ufa = ESeq.fromArray(new EObject[] {
										fun.module, fun.function, arg_list });
								return uf.apply(proc, ufa);
							}
						}
					},
							 getModuleClassLoader());
		}

		/**
		 * @param fun2
		 * @param value
		 */
		synchronized void add_export(EModule definer, FunID fun2, EFun value)
				throws Exception {
			this.resolved_value = value;
			this.defining_module = definer;

			for (FunctionBinder f : resolve_points) {
				// System.out.println("binding " + fun2 + " " + value + " -> " + f);
				f.bind(value);
			}
		}

		/**
		 * @return
		 * 
		 */
		public EFun resolve() {
			return getFunction();
		}

		/**
		 * @return
		 */
		public boolean exported() {
			return resolved_value != null;
		}
	}

	static class ModuleInfo {

		protected static final EAtom am_badfun = EAtom.intern("badfun");
		private EModule resident;
		private EAtom module;
		private EBinary module_md5 = empty_md5;
		Map<FunID, FunctionInfo> binding_points = new ConcurrentHashMap<FunID, FunctionInfo>();

		/**
		 * @param module
		 */
		public ModuleInfo(EAtom module) {
			this.module = module;
		}

		/**
		 * @param fun
		 * @param ref
		 * @return
		 * @throws Exception
		 */
		public void add_import(FunID fun, FunctionBinder ref) throws Exception {
			FunctionInfo info = get_function_info(fun);
			info.add_import(ref);
		}

		private synchronized FunctionInfo get_function_info(FunID fun) {
			FunctionInfo info = binding_points.get(fun);
			if (info == null) {
				binding_points.put(fun, info = new FunctionInfo(fun));
			}
			return info;
		}

		/**
		 * @param fun
		 * @param value
		 * @throws Exception
		 */
		public void add_export(EModule definer, FunID fun, EFun value)
				throws Exception {
			get_function_info(fun).add_export(definer, fun, value);
		}

		/**
		 * @param eModule
		 */
		public void setModule(EModule eModule) {
			this.resident = eModule;
		}

		/**
		 * @param start
		 * @return
		 */
		public EFun resolve(FunID fun) {
			return get_function_info(fun).resolve();
		}

		/**
		 * @param fun
		 * @return
		 */
		public boolean exports(FunID fun) {
			return get_function_info(fun).exported();
		}

		/**
		 * @return
		 */
		public boolean is_loaded() {
			return resident != null;
		}
		
		public void unload() throws Exception {
			resident = null;
			for (FunctionInfo fi : this.binding_points.values()) {
				fi.unbind();
			}
			binding_points.clear(); // ?
			this.resident = null;
			this.module_md5 = empty_md5;
		}

		/**
		 * @return
		 */
		public synchronized ESeq get_attributes() {
			ESeq res;
			
			if (resident == null)
				res = ERT.NIL;
			else
				res = resident.attributes();
			
			return res;
		}

		/**
		 * 
		 */
		public void warn_about_unresolved() {
			if (resident != null) {
				for (FunctionInfo fi : binding_points.values()) {
					if (fi.resolved_value == null) {
						log.log(Level.INFO, "unresolved after load: "+fi.fun);
					}
				}
			}
		}

		public ESeq get_exports() {
			ESeq rep = ERT.NIL;
			
			for (FunctionInfo fi : binding_points.values()) {
				if (fi.exported()) {
					rep = rep.cons(new ETuple2(fi.fun.function, ERT.box(fi.fun.arity)));
				}
			}

			return rep;
		}

		Map<Integer,EFunMaker> new_map = new HashMap<Integer, EFunMaker>();
		Map<Long,EFunMaker> old_map = new HashMap<Long, EFunMaker>();
		
		/** resolve a fun we got from a R5+ node 
		 * @param arity */
		public EFun resolve(EPID pid, EBinary md5, int index, final int old_uniq, final int old_index, int arity, EObject[] freevars) {
			if (resident == null) {				
				throw new NotImplemented("calling FUN before module "+this.module+" is loaded");
			}
			
			EFunMaker maker = new_map.get(index);
			
			if (maker == null || !md5.equals(module_md5)) {
				LocalFunID fid = new LocalFunID(module, ERT.am_undefined, arity, old_index, index, old_uniq, md5);
				return EFun.get_fun_with_handler(0, new EFunHandler() {					
					@Override
					public EObject invoke(EProc proc, EObject[] args) throws Pausable {
						throw new ErlangError(am_badfun, args);
					}
					
					@Override
					public String toString() {
						return "#Fun<" + module + "." +  old_index + "." + old_uniq + ">";
					}
				}, this.getClass().getClassLoader());
			}
			
			return maker.make(pid, freevars);
		}

		static final EBinary empty_md5 = new EBinary(new byte[16]);
		
		/** resolve a fun we got from a pre-R5 node */
		public EFun resolve(final EPID pid, final int old_uniq, final int old_index, final EObject[] freevars) {
			EFunMaker maker = old_map.get((((long)old_index)<<32)|(long)old_uniq);
			
			if (maker==null) {				
				LocalFunID fid = new LocalFunID(module, ERT.am_undef, 0, old_index, 0, old_uniq, empty_md5);
				return EFun.get_fun_with_handler(0, new EFunHandler() {					
					@Override
					public EObject invoke(EProc proc, EObject[] args) throws Pausable {
						throw new ErlangError(am_badfun, args);
					}
					
					@Override
					public String toString() {
						return "#Fun<" + module + "." +  old_index + "." + old_uniq + ">";
					}
				}, this.getClass().getClassLoader());
			}
			
			return maker.make(pid, freevars);
		}
		
		/** used to register a lambda/local function */
		public void register(LocalFunID fun_id, EFunMaker maker) {
			module_md5 = fun_id.new_uniq;
			new_map.put(fun_id.new_index, maker);
			old_map.put((((long)fun_id.index) << 32) | (long)fun_id.uniq , maker);
		}

	}

	public static void add_import(FunID fun, FunctionBinder ref) throws Exception {
		get_module_info(fun.module).add_import(fun, ref);
	}

	private static ModuleInfo get_module_info(EAtom module) {
		ModuleInfo mi;
		synchronized (infos) {
			mi = infos.get(module);
			if (mi == null) {
				infos.put(module, mi = new ModuleInfo(module));
			}
		}
		return mi;
	}

	public static void add_export(EModule mod, FunID fun, EFun value) throws Exception {
		get_module_info(fun.module).add_export(mod, fun, value);
	}

	// static private Map<EAtom, EModule> modules = new HashMap<EAtom,
	// EModule>();

	static void setup_module(EModule mod_inst) throws Error {

		ModuleInfo module_info = get_module_info(EAtom.intern(mod_inst.module_name()));
		module_info.setModule(mod_inst);

		try {
			mod_inst.registerImportsAndExports();
		} catch (Exception e) {
			throw new Error(e);
		}

		module_info.warn_about_unresolved();
	}
	
	public static void register_lambda(LocalFunID lambda_id, Class<? extends EFun> fun) {
		get_module_info(lambda_id.module).register(lambda_id, new EClassEFunMaker(fun));
	}



	/**
	 * @param start
	 * @return
	 */
	public static EFun resolve(FunID start) {
		return get_module_info(start.module).resolve(start);
	}
	
	public static EFun resolve(EPID pid, EAtom module, EBinary md5, int index, int old_uniq, int old_index, int arity, EObject[] freevars) {
		return get_module_info(module).resolve(pid, md5, index, old_uniq, old_index, arity, freevars);
	}

	public static EFun resolve(EPID pid, EAtom module, int old_uniq, int old_index, EObject[] freevars) {
		return get_module_info(module).resolve(pid, old_uniq, old_index, freevars);
	}

	/**
	 * @param m
	 * @param f
	 * @param value
	 * @return
	 */
	public static boolean function_exported(EAtom m, EAtom f, int a) {
		FunID fun = new FunID(m, f, a);
		return get_module_info(m).exports(fun);
	}

	/**
	 * @param m
	 * @return
	 */
	public static boolean module_loaded(EAtom m) {
		ModuleInfo mi = get_module_info(m);
		return mi.is_loaded();
	}

	/**
	 * @param mod
	 * @return
	 */
	public static ESeq get_attributes(EAtom mod) {
		ModuleInfo mi = get_module_info(mod);
		return mi.get_attributes();
	}

	/**
	 * @param mod
	 * @return
	 */
	public static ESeq get_exports(EAtom mod) {
		ModuleInfo mi = get_module_info(mod);
		return mi.get_exports();
	}

	public static abstract class FunctionBinder {
		public abstract void bind(EFun value) throws Exception;
		public abstract FunID getFunID();
	}

	public static ESeq loaded_modules() {
		EAtom[] mods;
		synchronized (infos) {
			mods = infos.keySet().toArray(new EAtom[0]);
		}
		ESeq out = ERT.NIL;
		for (int i = 0; i < mods.length; i++) {
			out = out.cons(mods[i]);
		}
		return out;
	}

	public static EAtom delete_module(EAtom module) {
		ModuleInfo mi = get_module_info(module);
		try {
			mi.unload();
		} catch (ErlangException e) {
			throw e;
		} catch (Exception e) {
			throw new ErlangError(e);
		}
		return ERT.am_ok;
	}

}
