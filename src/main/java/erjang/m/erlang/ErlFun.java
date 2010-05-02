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


package erjang.m.erlang;

import kilim.Pausable;
import erjang.BIF;
import erjang.EAtom;
import erjang.EFun;
import erjang.EFunHandler;
import erjang.EModuleManager;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.FunID;

/**
 * BIFs for fun_info/1, fun_info/2 and make_fun/3
 */
public class ErlFun {

	@BIF
	public static EObject is_function(EObject a1, EObject a2) {
		return a1.is_function(a2);
	}

	@BIF
	public static EObject function_exported(EObject a1, EObject a2, EObject a3) {
	    EAtom mod = a1.testAtom();
	    EAtom fun = a2.testAtom();
	    ESmall ary = a3.testSmall();

	    return ERT.box(EModuleManager.function_exported(mod,fun,ary.value));
	}

        
	
	@BIF
	public static EObject fun_info(EObject fun_arg, EObject spec_arg)
	{
		EFun fun = fun_arg.testFunction();
		EAtom spec = spec_arg.testAtom();
		
		if (fun == null || spec == null) throw ERT.badarg(fun_arg, spec_arg);
		
		return fun.info(spec);
	}
	
	@BIF
	public static ESeq fun_info(EObject fun_arg)
	{
		EFun fun = fun_arg.testFunction();
		if (fun == null) throw ERT.badarg(fun_arg);
		
		ESeq res = ERT.NIL;
		
		boolean is_local = fun.is_local();
		
		res = res.cons(fun.info(ERT.am_type));
		res = res.cons(fun.info(ERT.am_env));
		res = res.cons(fun.info(ERT.am_arity));
		res = res.cons(fun.info(ERT.am_name));
		
		if (is_local) {
			res = res.cons(fun.info(ERT.am_uniq));
			res = res.cons(fun.info(ERT.am_index));
			res = res.cons(fun.info(ERT.am_new_uniq));
			res = res.cons(fun.info(ERT.am_new_index));
		}
		
		res = res.cons(fun.info(ERT.am_module));
		
		if (is_local) {
			res = res.cons(fun.info(ERT.am_pid));
		}
		
		return res;
	}
	
	@BIF
	public static EFun make_fun(EObject m, EObject f, EObject a)
	{
		EAtom mod = m.testAtom();
		EAtom fun = f.testAtom();
		ESmall arity = a.testSmall();
		if (mod==null||fun==null||arity==null) {
			throw ERT.badarg(m,f,a);
		}
		
		return EModuleManager.resolve(new FunID(mod, fun, arity.value));
	}
	
}
