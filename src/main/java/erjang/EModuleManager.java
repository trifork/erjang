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

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;

import erjang.codegen.EFunCG;
import erjang.m.erlang.ErlBif;
import erjang.m.ets.EMatchSpec;
import erjang.m.java.JavaObject;
import kilim.Pausable;

/**
 * 
 */
public class EModuleManager {

	static Logger log = Logger.getLogger(EModuleManager.class.getName());
	
	static private Map<EAtom, ModuleInfo> infos = new ConcurrentHashMap<EAtom, ModuleInfo>();

	// static FunctionInfo undefined_function = null;
	static EAtom am_prep_stop = EAtom.intern("prep_stop");
	static EAtom am___info__ = EAtom.intern("__info__");
	static EAtom am_call = EAtom.intern("call");
	static EAtom am_trace = EAtom.intern("trace");

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
		boolean is_exported;

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
		synchronized void add_import(final FunctionBinder ref) {
			resolve_points.add(ref);
			if (is_exported && resolved_value != null) {
				// System.out.println("binding "+fun+" "+resolved_value+" -> "+ref);
				if (traceHandler == null) {
					ref.bind(resolved_value);
				} else {
					ref.bind(traceHandler.self);
				}
			} else {
				EFun h = getFunErrorHandler();
				ref.bind(h);
			}
		}
		
		synchronized void add_internal(final FunctionBinder ref) throws Exception {
			resolve_points.add(ref);
		}
		
		synchronized void unbind() throws Exception {
			for (FunctionBinder f : resolve_points) {
				f.bind(getFunErrorHandler());
			}
		}

		private EFun getTargetFunction() {
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
			return EFunCG.get_fun_with_handler(fun.module.getName(), fun.function.getName(), fun.arity,
                    new EFunHandler() {

                        @Override
                        public String toString() {
                            return "#EFunHandler<" + fun.toString() + ">";
                        }

                        public EObject invoke(EProc proc, EObject[] args)
                                throws Pausable {

                            /** Get reference to error_handler:undefined_function/3 */

                            EFun uf = proc.undefined_function.resolved_value;

                            /** this is just some debugging info to help understand downstream errors */
                            if (get_module_info(fun.module).is_loaded()) {
                                if (Boolean.getBoolean("erjang.declare_missing_imports") && fun.function != am_prep_stop && fun.function != am___info__)
                                    log.log(Level.INFO, "MISSING " + fun);
                            } else {
                                log.log(Level.FINER, "resolving" + fun);
                            }

                            Class<?> c = null;
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
                                log.log(Level.INFO, "failed to load " + fun + " (" + proc.undefined_function + " not found)");

                                throw new ErlangUndefined(fun.module,
                                        fun.function, fun.arity);
                            } else {
                                ESeq arg_list = ESeq.fromArray(args);
                                ESeq ufa = ESeq.fromArray(new EObject[]{
                                        fun.module, fun.function, arg_list});
                                return ErlBif.apply_last(proc, uf, ufa);
                                //	return uf.apply(proc, ufa);
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
			this.is_exported = true;
			this.defining_module = definer;
			bind(value);
		}
		
		void bind(EFun value) throws Exception {
			this.resolved_value = value;
			if (traceHandler == null) {
				for (FunctionBinder f : resolve_points) {
					// System.out.println("binding " + fun2 + " " + value + " -> " + f);
					f.bind(value);
				}
			} else {
				traceHandler.target = value;
			}
		}

		/**
		 * @return
		 * 
		 */
		public EFun resolve() {
			if (traceHandler == null) {
				return getTargetFunction();
			} else {
				return traceHandler.self;
			}
		}

		/**
		 * @return
		 */
		public boolean exported() {
			return resolved_value != null && is_exported;
		}

		public void trace_pattern(TraceSpec spec) {
			if (traceHandler == null) {
				traceHandler = new TraceHandler(this);
			}

			traceHandler.trace_spec = spec;
		}

		TraceHandler traceHandler;
	}

	static class TraceHandler implements EFunHandler {

		AtomicLong counter = new AtomicLong(0);
		TraceSpec trace_spec = new TraceSpec(ERT.FALSE, ERT.NIL);

		EFun target;
		final EFun self;

		TraceHandler(FunctionInfo h) {
			owner = h;
			target = h.getTargetFunction();
			self = EFunCG.get_fun_with_handler(
					h.fun.module.getName(),
					h.fun.function.getName(),
					h.fun.arity,
					this,
					TraceHandler.class.getClassLoader());

		}

		final FunctionInfo owner;

		@Override
		public EObject invoke(EProc proc, EObject[] args) throws Pausable {
			long state = before(proc, args);
			try {
				return target.invoke(proc, args);
			} finally {
				after(proc, state);
			}
		}

		long before(EProc proc, EObject[] args) throws Pausable {
			if (!proc.get_trace_flags().call || trace_spec.is_false) return 0;
			ESeq argList = null;
			if (trace_spec.is_true || trace_spec.ms.matches(proc, argList = EList.make((Object[])args))) {
				counter.incrementAndGet();

				// send {trace, Pid, call, {M, F, Args}}

				EObject arg = (proc.get_trace_flags().arity)
						? ERT.box(owner.fun.arity)
						: (argList == null ? EList.make((Object[])args) : argList);

				ERT.do_trace(proc, am_call, ETuple3.make_tuple(owner.fun.module, owner.fun.function, arg));
			}
			return 0;
		}

		void after(EProc proc, long state) {

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
		public void add_import(FunID fun, FunctionBinder ref) {
			FunctionInfo info = get_function_info(fun);
			info.add_import(ref);
		}

		public void add_internal(FunID fun, FunctionBinder ref) throws Exception {
			FunctionInfo info = get_function_info(fun);
			info.add_internal(ref);
		}

		synchronized FunctionInfo get_function_info(FunID fun) {
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
		 * @param fun
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
		
		public EAtom unload() throws Exception {
			
			if (resident == null) {
				return ERT.am_undefined;
			}
			
			resident = null;
			for (FunctionInfo fi : this.binding_points.values()) {
				fi.unbind();
			}
			binding_points.clear(); // ?
			this.resident = null;
			this.module_md5 = empty_md5;
			
			return ERT.TRUE;
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

		public synchronized ESeq get_compile() {
			ESeq res;
			
			if (resident == null)
				res = ERT.NIL;
			else
				res = resident.compile();
			
			return res;
		}

		/**
		 * 
		 */
		public void warn_about_unresolved() {
			if (resident != null) {
				for (FunctionInfo fi : binding_points.values()) {
					if ((fi.resolved_value == null) && (fi.fun.module != this.module)) {
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
				return EFunCG.get_fun_with_handler(this.module.getName(), "badfun", 0, new EFunHandler() {					
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
				return EFunCG.get_fun_with_handler(module.getName(), "badfun", 0, new EFunHandler() {					
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

		public void bind_nif(FunID id, EFunHandler handler) throws Exception {
			
			ClassLoader loader = this.getClass().getClassLoader();
			if (is_loaded()) {
				loader = resident.getModuleClassLoader();
			}
			
            EFun fun = EFunCG.get_fun_with_handler(
            		id.module.getName(), 
            		id.function.getName(), 
            		id.arity, 
            		handler, 
            		loader);
            
            FunctionInfo fi = get_function_info(id);
            
            fi.bind(fun);
            
		}

		public int trace_pattern(EAtom fun, int argi, TraceSpec spec) {
			ArrayList<FunctionInfo> fis = new ArrayList<FunctionInfo>();
			for ( Map.Entry<FunID,FunctionInfo> ent : binding_points.entrySet()) {
				FunID fid = ent.getKey();
				boolean is_fun = (fun == am__) || (fun == fid.function);
				boolean is_arg = argi == -1 || (argi == fid.arity);

				if (is_fun & is_arg) {
				  fis.add(ent.getValue());
				}
			}

			for (int i = 0; i < fis.size(); i++) {
				fis.get(i).trace_pattern(spec);
			}

			return fis.size();
		}

	}

	public static void add_import(FunID fun, FunctionBinder ref) {
		get_module_info(fun.module).add_import(fun, ref);
	}

	public static void add_internal(FunID fun, FunctionBinder ref) throws Exception {
		get_module_info(fun.module).add_internal(fun, ref);
	}

	static ModuleInfo get_module_info(EAtom module) {
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

	public static void bind_nif(FunID id, EFunHandler handler) throws Exception
	{
		get_module_info(id.module).bind_nif(id, handler);
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
	 * @param a
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

	public static ESeq get_compile(EAtom mod) {
		ModuleInfo mi = get_module_info(mod);
		return mi.get_compile();
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
		public abstract void bind(EFun value);
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
			return mi.unload();
		} catch (ErlangException e) {
			throw e;
		} catch (Exception e) {
			throw new ErlangError(e);
		}
	}

	public static EModule get_loaded_module(EAtom module) {
		ModuleInfo mi = get_module_info(module);
		return mi.resident;
	}


	static final EAtom am__ = EAtom.intern("_");

	public EModuleManager() {
	}

	public static int trace_pattern(EAtom mod, EAtom fun, EObject arg, EObject spec, EObject opts) {

		int argi = (arg == am__) ? -1 : arg.testSmall().asInt();

		ArrayList<ModuleInfo> mis = new ArrayList<ModuleInfo>();
		if (mod == am__) {
			ModuleInfo mi;
			synchronized (infos) {
				mis.addAll(infos.values());
			}
		} else {
			ModuleInfo mi = get_module_info(mod);
			mis.add(mi);
		}

		TraceSpec tspec = new TraceSpec(spec, opts);

		int count = 0;
		for (int i = 0; i < mis.size(); i++) {
			ModuleInfo mi = mis.get(i);
			count += mi.trace_pattern(fun, argi, tspec);
		}

		return count;
	}

	static class TraceSpec {

		boolean is_true;
		boolean is_false;
		EMatchSpec ms;
		public boolean do_count;

		TraceSpec(EObject spec, EObject opts) {
			ESeq seq = spec.testSeq();
			if (spec.isNil() || spec == ERT.TRUE) {
			    is_true = true;
			} else if (spec == ERT.FALSE) {
				is_false = true;
			} else if (seq != null) {
			   ms = EMatchSpec.compile(seq);
			} else {
			   throw ERT.badarg(spec);
			}

		}

	}
}
