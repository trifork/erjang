package erjang.m.java;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import kilim.Pausable;

import erjang.BIF;
import erjang.EAtom;
import erjang.EFun;
import erjang.ENative;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ErlangError;

public class Native extends ENative {

	@BIF
	static EObject get_static(EProc self, EObject clzz, EObject member) {

		EAtom clz_am = clzz.testAtom();
		EAtom mem_am = member.testAtom();

		if (clz_am == null || mem_am == null)
			throw ERT.badarg(clzz, member);

		try {

			Class<?> c = Class.forName(clz_am.getName());
			Field f = c.getField(mem_am.getName());

			if (java.lang.reflect.Modifier.isStatic(f.getModifiers())) {
				return JavaObject.box(self, f.get(null));
			} else {
				throw new ErlangError(EString.fromString("not a static field"),
						clzz, member);
			}

		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), clzz,
					member);
		}
	}

	@BIF
	static EObject call(EProc self, EObject obj, EObject member, EObject typez, EObject argz) {

		EAtom mem_am = member.testAtom();
		ESeq type_seq = typez.testSeq();
		ESeq arg_seq = argz.testSeq();

		Object receiver = JavaObject.unbox(self, Object.class, obj);

		if (mem_am == null || type_seq == null || arg_seq == null
				|| type_seq.length() != arg_seq.length() || receiver == null)
			throw ERT.badarg(obj, member, typez, argz);

		try {
			Class<?> c = receiver.getClass();
			Class<?>[] arg_types = new Class<?>[type_seq.length()];
			EObject[] at = type_seq.toArray();

			for (int i = 0; i < at.length; i++) {
				EAtom am = at[i].testAtom();
				if (am == null) {
					throw ERT.badarg(obj, member, typez, argz);
				}

				arg_types[i] = Class.forName(am.getName());
			}

			Method m = c.getMethod(mem_am.getName(), arg_types);

			Object res = m.invoke(receiver, JavaObject.convert_args(self, arg_types,
					arg_seq));

			if (m.getReturnType() == Void.TYPE) {
				return ERT.am_ok;
			}

			return JavaObject.box(self, res);

			/*
			 * } catch (ClassNotFoundException e) { // TODO Auto-generated catch
			 * block e.printStackTrace(); } catch (SecurityException e) { //
			 * TODO Auto-generated catch block e.printStackTrace(); } catch
			 * (NoSuchMethodException e) { // TODO Auto-generated catch block
			 * e.printStackTrace();
			 * 
			 * 
			 * } catch (IllegalArgumentException e) { // TODO Auto-generated
			 * catch block e.printStackTrace(); } catch (IllegalAccessException
			 * e) { // TODO Auto-generated catch block e.printStackTrace(); }
			 * catch (InvocationTargetException e) { // TODO Auto-generated
			 * catch block e.printStackTrace();
			 */

		} catch (Exception e) {
			throw new ErlangError(EString.fromString(e.getMessage()), obj,
					member, typez, argz);
		} finally {

		}

	}
	
	@BIF
	public static EObject run(EProc proc, EObject fun) throws Pausable {
		EFun f = fun.testFunction2(0);
		if (f == null) {
			throw ERT.badarg(fun);
		}
		
		return f.invoke(proc, new EObject[0]);
	}

}
