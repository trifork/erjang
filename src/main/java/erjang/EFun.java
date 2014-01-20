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

import kilim.Pausable;

import erjang.beam.EUtil;
import erjang.m.ets.EMatchContext;
import erjang.m.ets.ETermPattern;

/** Representation of an Erlang function object.
 * At present this class also contains a lot of code generation code for
 * EFun's dynamically-created subclasses.
 *
 * Please see <code>doc/RuntimeClassHierarchy.md</code> for more about
 * the dynamically-created classes.
 */
public abstract class EFun extends EObject {

	public abstract int arity();

	@BIF
	public EAtom is_function(EObject arity) {
		ESmall ary;
		if ((ary=arity.testSmall()) != null) {
			return ERT.box( arity() == ary.value );
		}
		throw ERT.badarg(this, arity);
	}
	

	public EFun testFunction2(int nargs) {
		if (this.arity() == nargs)
			return this;
		return null;
	}

	public EFun testFunction() {
		return this;
	}

	@Override
	int cmp_order() {
		return CMP_ORDER_FUN;
	}
	
	/* (non-Javadoc)
	 * @see erjang.EObject#compare_same(erjang.EObject)
	 */
	@Override
	int compare_same(EObject rhs) {
		if (rhs == this) return 0;
		return (System.identityHashCode(this)&0xffff)
			 - (System.identityHashCode(rhs)&0xffff);
	}

	/** used for translation of tail recursive methods */
	public EObject go(EProc eproc) throws Pausable { return go2(eproc); }

	/** used for translation of tail recursive methods */
	public EObject go2(EProc eproc) {
		throw new java.lang.AbstractMethodError(this.getClass().getName() + "#go2(EProc)");
	}

    /**
     * @param a
     * @return
     */
    public EObject apply(EProc proc, ESeq a) throws Pausable {
        // TODO: this should be implemented for all EFunX
        return invoke(proc, a.toArray());
    }

    /** generic invoke, used only for apply */
	public abstract EObject invoke(EProc proc, EObject[] args) throws Pausable;

	/** pass-thru handler for interpreter */
	public EObject invoke(EProc proc, EObject[] args, int off, int len) 
		throws Pausable 
	{
		EObject[] new_args = new EObject[len];
		System.arraycopy(args, off, new_args, 0, len);
		return invoke(proc, new_args);
	}

	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	/**
	 * @param eInputStream
	 * @return
	 * @throws IOException
	 */
	public static EFun read(EInputStream eis) throws IOException {
		return eis.read_fun();
	}

	/**
	 * @param spec
	 * @return
	 */
	public EObject info(EAtom spec) {

		FunID id = get_id();

		if (spec == ERT.am_arity) {
			return new ETuple2(spec, ERT.box(id.arity - get_env().length()));
		} else if (spec == ERT.am_module) {
			return new ETuple2(spec, id.module);
		} else if (spec == ERT.am_name) {
			return new ETuple2(spec, id.function);
		} else if (spec == ERT.am_env) {
			return new ETuple2(spec, this.get_env());
		} else if (spec == ERT.am_type) {
			return new ETuple2(ERT.am_type, (id instanceof LocalFunID) ? ERT.am_local : ERT.am_external);
		}

		if (id instanceof LocalFunID) {
			LocalFunID lid = (LocalFunID) id;
			if (spec == ERT.am_index) {
				return new ETuple2(spec, ERT.box(lid.index));
			} else if (spec == ERT.am_new_index) {
				return new ETuple2(spec, ERT.box(lid.new_index));
			} else if (spec == ERT.am_uniq) {
				return new ETuple2(spec, ERT.box(lid.uniq));
			} else if (spec == ERT.am_new_uniq) {
				return new ETuple2(spec, lid.new_uniq);
			} else if (spec == ERT.am_pid) {
				return new ETuple2(spec, this.get_pid());
			}
		} else {
			if (spec == ERT.am_type) {
				return new ETuple2(ERT.am_type, ERT.am_external);
			}
		}

		return ERT.am_undefined;
	}

	public boolean is_local() {
		return this.get_id() instanceof LocalFunID;
	}

	protected EObject get_pid() {
		return ERT.NIL;
	}

	protected ESeq get_env() {
		return ERT.NIL;
	}

	@Override
	public String toString() {
		FunID id = get_id();
		if (id instanceof LocalFunID) {
			LocalFunID lid = (LocalFunID) id;
			return "#Fun<" + id.module + "." + lid.index + "." + lid.uniq + ">";
		} else {
			return "#Fun<" + id.module + ":" + id.function + "/" + id.arity + ">";
		}
	}

    private static final String ERJANG_MODULES_DOT = "erjang.m.";

    protected FunID get_id() {
		String cname = getClass().getName();
		EAtom module = null;
		if (cname.startsWith(ERJANG_MODULES_DOT)) {

			int last = cname.lastIndexOf('.');
			String name = cname.substring(ERJANG_MODULES_DOT.length(),
					last);
			module = EAtom.intern(EUtil.decodeJavaName(name));
		}

		EAtom fun = null;
		int end = cname.lastIndexOf("__");
		int start = cname.indexOf("$FN_");
		if (start != 1 && end != -1) {
			String method_name = cname.substring(start+4, end);
			fun = EAtom.intern(EUtil.decodeJavaName(method_name));
		}

		return new FunID(module, fun, arity());
	}

}
