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

package erjang.m.ets;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import com.trifork.clj_ds.IPersistentCollection;
import com.trifork.clj_ds.ISeq;

import erjang.EAtom;
import erjang.EBitString;
import erjang.ECons;
import erjang.EFun;
import erjang.ENumber;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EPseudoTerm;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.ETuple;
import erjang.ETuple3;
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.NotImplemented;
import erjang.beam.BIFUtil;
import erjang.beam.BuiltInFunction;

/**
 * Base class for a parsed match_spec.
 * 
 * @see http://ftp.sunet.se/pub/lang/erlang/doc/apps/erts/match_spec.html
 */
public class EMatchSpec extends EPseudoTerm {
	static final EAtom am_set_seq_token = EAtom.intern("set_seq_token");
	static final EAtom am_get_seq_token = EAtom.intern("get_seq_token");
	static final EAtom am_message = EAtom.intern("message");
	static final EAtom am_return_trace = EAtom.intern("return_trace");
	static final EAtom am_exception_trace = EAtom.intern("exception_trace");
	static final EAtom am_process_dump = EAtom.intern("process_dump");
	static final EAtom am_enable_trace = EAtom.intern("enable_trace");
	static final EAtom am_disable_trace = EAtom.intern("disable_trace");
	static final EAtom am_trace = EAtom.intern("trace");
	static final EAtom am_display = EAtom.intern("display");
	static final EAtom am_caller = EAtom.intern("caller");
	static final EAtom am_set_tcw = EAtom.intern("set_tcw");
	static final EAtom am_silent = EAtom.intern("silent");

	static final Set<EAtom> ActionFunctions = new HashSet<EAtom>();
	static {
		Collections.addAll(ActionFunctions, am_set_seq_token, am_get_seq_token,
				am_message, am_return_trace, am_exception_trace,
				am_process_dump, am_enable_trace, am_disable_trace, am_trace,
				am_display, am_caller, am_set_tcw, am_silent);
	}

	static final EAtom am_is_atom = EAtom.intern("is_atom");
	static final EAtom am_is_constant = EAtom.intern("is_constant");
	static final EAtom am_is_float = EAtom.intern("is_float");
	static final EAtom am_is_integer = EAtom.intern("is_integer");
	static final EAtom am_is_list = EAtom.intern("is_list");
	static final EAtom am_is_number = EAtom.intern("is_number");
	static final EAtom am_is_pid = EAtom.intern("is_pid");
	static final EAtom am_is_port = EAtom.intern("is_port");
	static final EAtom am_is_reference = EAtom.intern("is_reference");
	static final EAtom am_is_tuple = EAtom.intern("is_tuple");
	static final EAtom am_is_binary = EAtom.intern("is_binary");
	static final EAtom am_is_function = EAtom.intern("is_function");
	static final EAtom am_is_record = EAtom.intern("is_record");
	static final EAtom am_is_seq_trace = EAtom.intern("is_seq_trace");
	static final EAtom am_and = EAtom.intern("and");
	static final EAtom am_or = EAtom.intern("or");
	static final EAtom am_not = EAtom.intern("not");
	static final EAtom am_xor = EAtom.intern("xor");
	static final EAtom am_andalso = EAtom.intern("andalso");
	static final EAtom am_orelse = EAtom.intern("orelse");

	static final Set<EAtom> BoolFunctions = new HashSet<EAtom>();
	static {
		Collections.addAll(BoolFunctions, am_is_atom, am_is_constant,
				am_is_float, am_is_integer, am_is_list, am_is_number,
				am_is_pid, am_is_port, am_is_reference, am_is_tuple,
				am_is_binary, am_is_function, am_is_record, am_is_seq_trace,
				am_and, am_or, am_not, am_xor, am_andalso, am_orelse);
	}

/**
	 * abs | element | hd | length | node | round | size | tl | trunc | '+' | '-' 
	 * | '*' | 'div' | 'rem' | 'band' | 'bor' | 'bxor' | 'bnot' | 'bsl' | 'bsr' 
	 * | '>' | '>=' | '<' | '=<' | '=:=' | '==' | '=/=' | '/=' | self | get_tcw
	 */

	static final EAtom am_abs = EAtom.intern("abs");
	static final EAtom am_element = EAtom.intern("element");
	static final EAtom am_hd = EAtom.intern("hd");
	static final EAtom am_length = EAtom.intern("length");
	static final EAtom am_node = EAtom.intern("node");
	static final EAtom am_round = EAtom.intern("round");
	static final EAtom am_size = EAtom.intern("size");
	static final EAtom am_tl = EAtom.intern("tl");
	static final EAtom am_trunc = EAtom.intern("trunc");
	static final EAtom am_PLUS = EAtom.intern("+");
	static final EAtom am_MINUS = EAtom.intern("-");
	static final EAtom am_ASTERISK = EAtom.intern("*");
	static final EAtom am_div = EAtom.intern("div");
	static final EAtom am_rem = EAtom.intern("rem");
	static final EAtom am_band = EAtom.intern("band");
	static final EAtom am_bor = EAtom.intern("bor");
	static final EAtom am_bxor = EAtom.intern("bxor");
	static final EAtom am_bnor = EAtom.intern("bnor");
	static final EAtom am_bsl = EAtom.intern("bsl");
	static final EAtom am_bsr = EAtom.intern("bsr");
	static final EAtom am_GT = EAtom.intern(">");
	static final EAtom am_GE = EAtom.intern(">=");
	static final EAtom am_LT = EAtom.intern("<");
	static final EAtom am_LE = EAtom.intern("=<");
	static final EAtom am_EEQ = EAtom.intern("=:=");
	static final EAtom am_EQ = EAtom.intern("==");
	static final EAtom am_ENE = EAtom.intern("=/=");
	static final EAtom am_NE = EAtom.intern("/=");
	static final EAtom am_self = EAtom.intern("self");
	static final EAtom am_get_tcw = EAtom.intern("get_tcw");

	static final Set<EAtom> GuardFunctions = new HashSet<EAtom>();
	static {
		GuardFunctions.addAll(BoolFunctions);
		Collections.addAll(GuardFunctions, am_abs, am_element, am_hd,
				am_length, am_node, am_round, am_size, am_tl, am_trunc,
				am_PLUS, am_MINUS, am_ASTERISK, am_div, am_rem, am_band,
				am_bor, am_bxor, am_bsl, am_bsr, am_GT, am_GE, am_LT, am_LE,
				am_EEQ, am_EQ, am_ENE, am_NE, am_self, am_get_tcw);
	}

	private static final EAtom am_ANY = EAtom.intern("_");
	private static final EAtom am_ALL_VARS = EAtom.intern("$$");
	private static final EAtom am_ENTIRE_MATCH = EAtom.intern("$_");
	private static final EAtom am_const = EAtom.intern("const");

	static java.util.regex.Pattern MATCH_VAR = java.util.regex.Pattern
			.compile("^\\$([0-9]+)$");

	static final boolean is_match_var(EAtom am) {
		String name = am.getName();
		return name.length() > 0 && name.charAt(0) == '$' &&
			MATCH_VAR.matcher(name).matches();
	}

	static final boolean is_expr_match_var(EAtom am) {
		return am == am_ALL_VARS || am == am_ENTIRE_MATCH || is_match_var(am);
	}

	private final MatchFunction[] funs;
	private ESeq spec;

	static abstract class Pattern extends ETermPattern {
		abstract boolean is_simple();
		// that's it.
	}

	/** matches when a value is == */
	static class EqualsPattern extends Pattern {

		final EObject value;

		boolean is_simple() { return true; }

		/**
		 * @param part
		 */
		public EqualsPattern(EObject term) {
			this.value = term;
		}

		@Override
		public boolean match(ECons c, EMatchContext r) {
			return c.erlangEquals(value);
		}

		@Override
		public boolean match(ETuple t, EMatchContext r) {
			return t.erlangEquals(value);
		}

		public boolean match(EPID pid, EMatchContext r) {
			return pid.erlangEquals(value);
		}

		public boolean match(ERef ref, EMatchContext r) {
			return ref.erlangEquals(value);
		}

		public boolean match(EPort port, EMatchContext r) {
			return port.erlangEquals(value);
		}

		public boolean match(EAtom am, EMatchContext r) {
			return am.erlangEquals(value);
		}

		public boolean match(EFun fu, EMatchContext r) {
			return fu.erlangEquals(value);
		}

		public boolean match(EBitString bits, EMatchContext r) {
			return bits.erlangEquals(value);
		}

		public boolean match(ENumber num, EMatchContext r) {
			return num.erlangEquals(value);
		}

	}

	static abstract class Expr {

		public static final Expr[] EMPTY_ARR = new Expr[0];

		/**
		 * @param ctx
		 * @return
		 */
		public abstract EObject eval(EMatchContext ctx);

	}

	static class ActionCall extends Expr {

		EAtom action;
		Expr[] args;

		public ActionCall(ETuple t, ParseContext ctx) {

			assert (ActionFunctions.contains(t.elm(1)));

			action = t.elm(1).testAtom();
			args = new Expr[t.arity() - 1];

			for (int i = 2; i <= t.arity(); i++) {
				args[i - 2] = parse_ActionTerm(t.elm(i), ctx);
			}
		}

		@Override
		public EObject eval(EMatchContext ctx) {
			EObject[] vals = new EObject[args.length];
			for (int i = 0; i < vals.length; i++) {
				vals[i] = args[i].eval(ctx);
			}

			// for debug output
			List<EObject> aa = new ArrayList<EObject>();
			for (int i = 0; i < vals.length; i++) {
				aa.add(vals[i]);
			}
			throw new NotImplemented("ActionCall " + action + " " + aa);
		}

	}

	static class TupleConstruct extends Expr {

		final Expr[] elems;

		public TupleConstruct(ETuple innerT, ParseContext ctx) {
			elems = new Expr[innerT.arity()];
			for (int idx1 = 1; idx1 <= innerT.arity(); idx1++) {
				elems[idx1 - 1] = parse_ConditionExpression(innerT.elm(idx1),
						ctx);
			}
		}

		@Override
		public EObject eval(EMatchContext ctx) {
			ETuple res = ETuple.make(elems.length);
			for (int idx0 = 0; idx0 < elems.length; idx0++) {
				res.set(idx0 + 1, elems[idx0].eval(ctx));
			}
			return res;
		}

	}

	static class ConsConstruct extends Expr {

		Expr head_expr, tail_expr;

		public ConsConstruct(ECons seq, ParseContext ctx) {
			head_expr = parse_ConditionExpression(seq.head(), ctx);
			tail_expr = parse_ConditionExpression(seq.tail(), ctx);
		}

		@Override
		public EObject eval(EMatchContext ctx) {
			EObject head = head_expr.eval(ctx);
			EObject tail = tail_expr.eval(ctx);
			return tail.cons(head);
		}
	}

	static class ConstantExpr extends Expr {

		final EObject term;

		ConstantExpr(EObject term) {
			this.term = term;
		}

		@Override
		public EObject eval(EMatchContext ctx) {
			return term;
		}
	}

	/** $<x> appearing in conditions or body */
	static class MatchVarExpr extends Expr {

		final int var_name;

		// special index for '$$'
		static final int EXPR_ALL_VARS = -1;

		// special index for '$_'
		static final int EXPR_ENTIRE_MATCH = -2;

		MatchVarExpr(EAtom var, ParseContext ctx) {
			if (var == am_ALL_VARS) {
				var_name = EXPR_ALL_VARS;
			} else if (var == am_ENTIRE_MATCH) {
				var_name = EXPR_ENTIRE_MATCH;
			} else {
				int idx = Integer.parseInt(var.getName().substring(1));
				var_name = idx;
				if (var_name > 100000000 || var_name < 0) {
					throw new IllegalArgumentException("given index " + idx
							+ " is out of range");
				}
				if (!ctx.isBound(var_name)) {
					throw new IllegalArgumentException("unbound variable $"
							+ idx);
				}
			}
		}

		@Override
		public EObject eval(EMatchContext ctx) {
			if (var_name == EXPR_ENTIRE_MATCH) {
				return ctx.value;
			} else if (var_name == EXPR_ALL_VARS) {
				return ctx.makeList();
			} else {
				EObject value = ctx.vars.get(var_name);
				if (value == null)
					throw new InternalError("Unbound $"+var_name);
				return value;				
			}
		}
	}

	/**
	 * 
	 */
	static class MatchFunction {

		private final Pattern head;
		private final GuardCall[] cond;
		private final Expr[] body;
		private final Integer[] nvars;

		public MatchFunction(Pattern head, GuardCall[] cond, Expr[] body,
				Integer[] nvars) {
			this.head = head;
			this.cond = cond;
			this.body = body;
			this.nvars = nvars;
		}

		/**
		 * @param prev
		 *            is the list of previous matches
		 * @param value
		 *            is the value to match
		 * @return (cons <code>result</code> prev) if value matches; otherwise
		 *         prev
		 * */
		EObject match(EObject value) {
			EMatchContext ctx = new EMatchContext(nvars, value);
			if (!value.match(head, ctx)) {
				return null;
			}

			// exception during condition evaluation
			// just makes the match fail silently
			try {
				for (int i = 0; i < cond.length; i++) {
					if (!cond[i].test(ctx))
						return null;
				}
			} catch (Exception e) {
				return null;
			}

			EObject out = value;
			try {
				for (int i = 0; i < body.length; i++) {
					out = body[i].eval(ctx);
				}
			} catch (Exception e) {
				e.printStackTrace();
				out = ERT.am_EXIT;
			}

			return out;
		}

	}

	static class GuardCall extends Expr {

		EAtom guard;
		Expr[] args;
		private BuiltInFunction bif;

		/**
		 * @param t
		 * @param ctx
		 */
		public GuardCall(ETuple t, ParseContext ctx) {

			if (t.arity() < 1 || !GuardFunctions.contains(t.elm(1))) {
				throw ERT.badarg();
			}

			guard = t.elm(1).testAtom();
			args = new Expr[t.arity() - 1];

			for (int i = 2; i <= t.arity(); i++) {
				args[i - 2] = parse_ConditionExpression(t.elm(i), ctx);
			}
			
			this.bif = BIFUtil.getMethod("erlang", guard.getName(), args.length, true, false);
		}

		/**
		 * @param ctx
		 * @return
		 */
		public boolean test(EMatchContext ctx) {
			EObject[] vals = new EObject[args.length];
			for (int i = 0; i < vals.length; i++) {
				vals[i] = args[i].eval(ctx);
			}

			if (bif != null) {
				try {
					Method m = bif.javaMethod;
					if (bif.isVirtual) {
						Object[] vargs = new Object[vals.length-1];
						System.arraycopy(vals, 1, vargs, 0, vals.length-1);
						return m.invoke(vals[0], (Object[])vargs) == ERT.TRUE;
					} else {
						return m.invoke(null, (Object[])vals) == ERT.TRUE;
					}
				} catch (ErlangException e) {
					throw e;
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
					return false;
				} catch (IllegalAccessException e) {
					e.printStackTrace();
					return false;
				} catch (InvocationTargetException e) {
					e.printStackTrace();
					return false;
				}
			} else {				
				// for debug output
				List<EObject> aa = new ArrayList<EObject>();
				for (int i = 0; i < vals.length; i++) {
					aa.add(vals[i]);
				}
				
				
				throw new NotImplemented("GuardCall:test " + guard + " " + aa);
			}
		}

		public static final GuardCall[] EMPTY_ARR = new GuardCall[0];

		@Override
		public EObject eval(EMatchContext ctx) {
			EObject[] vals = new EObject[args.length];
			for (int i = 0; i < vals.length; i++) {
				vals[i] = args[i].eval(ctx);
			}

			if (bif != null) {
				try {
					Method m = bif.javaMethod;
					if (bif.isVirtual) {
						Object[] vargs = new Object[vals.length-1];
						System.arraycopy(vals, 1, vargs, 0, vals.length-1);
						return (EObject) m.invoke(vals[0], (Object[])vargs);
					} else {
						return (EObject) m.invoke(null, (Object[])vals);
					}
				} catch (ErlangException e) {
					throw e;
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
					throw new ErlangError(e);
				} catch (IllegalAccessException e) {
					e.printStackTrace();
					throw new ErlangError(e);
				} catch (InvocationTargetException e) {
					e.printStackTrace();
					throw new ErlangError(e);
				}
			} else {				
				// for debug output
				List<EObject> aa = new ArrayList<EObject>();
				for (int i = 0; i < vals.length; i++) {
					aa.add(vals[i]);
				}
				throw new NotImplemented("GuardCall:test " + guard + " " + aa);
			}

		}
	}

	enum ParseMode {
		HEAD, CONDITIONS, BODY
	}

	static class ParseContext {
		private Set<Integer> bound = new TreeSet<Integer>();
		private ParseMode mode;

		public boolean isBound(int idx0) {
			return bound.contains(idx0);
		}

		public void bind(MatchVariable matchVariable, int idx0) {
			bound.add(idx0);
		}

		/**
		 * @param head
		 */
		public void setMode(ParseMode mode) {
			this.mode = mode;
		}

		/**
		 * @return
		 */
		public Integer[] getNumberVars() {
			return bound.toArray(new Integer[bound.size()]);
		}

	}

	static MatchFunction parse_MatchFunction(EObject fspec) {
		ETuple3 tup = ETuple3.cast(fspec);
		if (tup == null)
			throw ERT.badarg(fspec);

		ParseContext ctx = new ParseContext();

		ctx.setMode(ParseMode.HEAD);
		Pattern head = parse_MatchHead(tup.elem1, ctx);
		ctx.setMode(ParseMode.CONDITIONS);
		GuardCall[] cond = parse_MatchConditions(tup.elem2.testSeq(), ctx);
		ctx.setMode(ParseMode.BODY);
		Expr[] body = parse_MatchBody(tup.elem3.testSeq(), ctx);

		return new MatchFunction(head, cond, body, ctx.getNumberVars());
	}

	private static Expr[] parse_MatchBody(ESeq actionTerms, ParseContext ctx) {
		if (actionTerms == null)
			throw ERT.badarg();

		if (actionTerms == ERT.NIL) {
			return Expr.EMPTY_ARR;
		}

		List<Expr> terms = new ArrayList<Expr>(actionTerms.length());
		for (ESeq actions = actionTerms; !actions.isNil(); actions = actions
				.tail()) {
			EObject action = actions.head();
			terms.add(parse_ActionTerm(action, ctx));
		}

		return terms.toArray(new Expr[terms.size()]);
	}

	private static Expr parse_ActionTerm(EObject action, ParseContext ctx) {
		ETuple t = action.testTuple();
		if (t != null && t.arity() >= 1 && ActionFunctions.contains(t.elm(1))) {
			return new ActionCall(t, ctx);
		} else {
			return parse_ConditionExpression(action, ctx);
		}

	}

	/**
	 * @param action
	 * @param ctx
	 * @return
	 */
	private static Expr parse_ConditionExpression(EObject expr, ParseContext ctx) {

		EAtom am = expr.testAtom();
		if (am != null && is_expr_match_var(am)) {
			return parse_ExprMatchVariable(am, ctx);

		}

		ETuple t = expr.testTuple();
		if (t != null && t.arity() >= 1 && GuardFunctions.contains(t.elm(1))) {
			return parse_MatchCondition(t, ctx);
		}

		return parse_TermConstruct(expr, ctx);
	}

	/**
	 * @param expr
	 * @param ctx
	 * @return
	 */
	private static Expr parse_TermConstruct(EObject expr, ParseContext ctx) {

		ETuple t = expr.testTuple();
		if (t != null) {

			// {const, X}
			if (t.arity() == 2 && t.elm(1) == am_const) {
				return new ConstantExpr(t.elm(2));
			}

			// {{}} | {{ConditionExpression, ...}}
			ETuple inner_t;
			if (t.arity() == 1 && (inner_t = t.elm(1).testTuple()) != null) {
				return new TupleConstruct(inner_t, ctx);
			}

			throw ERT.badarg();
		}

		// [] | [ConditionExpression, ...]
		ECons cons = expr.testCons();
		if (cons != null && !cons.isNil()) {
			// nil falls thru to the ConstantExpr below

			return new ConsConstruct(cons, ctx);
		}

		// NonCompositeTerm
		return new ConstantExpr(expr);
	}

	/**
	 * @param am
	 * @param ctx
	 * @return
	 */
	private static Expr parse_ExprMatchVariable(EAtom am, ParseContext ctx) {
		assert is_match_var(am);

		return new MatchVarExpr(am, ctx);
	}

	private static GuardCall[] parse_MatchConditions(ESeq conds,
			ParseContext ctx) {
		if (conds == null)
			throw ERT.badarg();

		if (conds.isNil()) {
			return GuardCall.EMPTY_ARR;
		}

		List<GuardCall> res = new ArrayList<GuardCall>();
		while (!conds.isNil()) {
			res.add(parse_MatchCondition(conds.head(), ctx));
			conds = conds.tail();
		}

		return res.toArray(new GuardCall[res.size()]);
	}

	private static GuardCall parse_MatchCondition(EObject expr, ParseContext ctx) {
		ETuple t = expr.testTuple();
		if (t == null || t.arity() < 1 || !GuardFunctions.contains(t.elm(1))) {
			throw ERT.badarg(expr);
		}

		return new GuardCall(t, ctx);
	}

	private static Pattern parse_MatchHead(EObject head, ParseContext ctx) {

		Pattern p = compile_Match(head, ctx);
		if (p != null)
			return p;

		throw ERT.badarg(head);

	}

	private static Pattern compile_Match(EObject head, ParseContext ctx) {
		EAtom am;
		ECons cons;
		ETuple tuple;

		// is it '_'
		if (head == am_ANY) {
			return (AnyPattern.INSTANCE);

			// is it '$<idx>' ?
		} else if ((am = head.testAtom()) != null && is_match_var(am)) {
			return (parse_MatchVariable(am, ctx));

		} else if (head.isNil()) {
			return NilPattern.INSTANCE;

			// is it [MatchHeadPart, ...]
		} else if ((cons = head.testCons()) != null) {
			return (new ConsPattern(cons, ctx));

		} else if ((tuple = head.testTuple()) != null) {
			TuplePattern tp = new TuplePattern(tuple, ctx);
			if (tp.is_simple()) {
				return new EqualsPattern(tuple);
			} else {
				return tp;
			}
		}

		return null;
	}

	static class TuplePattern extends Pattern {
		Pattern[] elems;
		
		boolean is_simple() { 
			for (int i = 0; i < elems.length; i++) {
				if (!elems[i].is_simple()) {
					return false;
				}
			}
			return true;
		}


		public TuplePattern(ETuple tuple, ParseContext ctx) {
			elems = new Pattern[tuple.arity()];
			for (int idx1 = 1; idx1 <= tuple.arity(); idx1++) {
				elems[idx1 - 1] = parse_MatchHeadPart(tuple.elm(idx1), ctx);
			}
		}

		@Override
		public boolean match(ETuple t, EMatchContext r) {
			if (t.arity() != elems.length)
				return false;
			for (int idx0 = 0; idx0 < elems.length; idx0++) {
				if (!t.elm(idx0 + 1).match(elems[idx0], r))
					return false;
			}
			return true;
		}
	}

	static class NilPattern extends Pattern {
		static final NilPattern INSTANCE = new NilPattern();

		boolean is_simple() { return true; }

		@Override
		public boolean match(ECons c, EMatchContext r) {
			return c.isNil();
		}
	}

	static class ConsPattern extends Pattern {
		Pattern head_p, tail_p;

		boolean is_simple() { return head_p.is_simple() && tail_p.is_simple(); }

		public ConsPattern(ECons cons, ParseContext ctx) {
			head_p = parse_MatchHeadPart(cons.head(), ctx);
			tail_p = parse_MatchHeadPart(cons.tail(), ctx);
		}

		@Override
		public boolean match(ECons c, EMatchContext r) {
			if (c.isNil())
				return false;
			return c.head().match(head_p, r) && c.tail().match(tail_p, r);
		}
	}

	/**
	 * Equal to parse_MatchHead, except last case yields an EqualsPattern
	 */
	private static Pattern parse_MatchHeadPart(EObject part, ParseContext ctx) {
		Pattern p = compile_Match(part, ctx);
		if (p != null)
			return p;

		// else, any other term can be used as a "TermPattern"
		return new EqualsPattern(part);
	}

	private static Pattern parse_MatchVariable(EAtom am, ParseContext ctx) {
		return new MatchVariable(am, ctx);
	}

	static class AnyPattern extends Pattern {
		static AnyPattern INSTANCE = new AnyPattern();

		boolean is_simple() { return false; }

		public boolean match(ETuple t, EMatchContext r) {
			return true;
		}

		public boolean match(ENumber n, EMatchContext r) {
			return true;
		}

		public boolean match(EAtom a, EMatchContext r) {
			return true;
		}

		public boolean match(EFun a, EMatchContext r) {
			return true;
		}

		public boolean match(ERef a, EMatchContext r) {
			return true;
		}

		public boolean match(ECons c, EMatchContext r) {
			return true;
		}

		public boolean match(EPID p, EMatchContext r) {
			return true;
		}

		public boolean match(EPort p, EMatchContext r) {
			return true;
		}

		public boolean match(EBitString bs, EMatchContext r) {
			return true;
		}

	}

	static class MatchVariable extends Pattern {

		final boolean free;
		final Integer var_name;

		boolean is_simple() { return false; }

		/**
		 * @param am
		 * @param ctx
		 */
		public MatchVariable(EAtom var, ParseContext ctx) {
			int name = Integer.parseInt(var.getName().substring(1));
			this.var_name = name;
			if (name > 100000000 || name < 0) {
				throw new IllegalArgumentException("given index " + name
						+ " is out of range");
			}
			free = !ctx.isBound(name);

			if (free)
				ctx.bind(this, name);
		}

		public boolean match(ETuple t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(ENumber t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(EAtom t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(ERef ref, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, ref);
				return true;
			} else {
				return ref.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(EFun fu, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, fu);
				return true;
			} else {
				return fu.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(ECons t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(EPID t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(EPort t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}

		public boolean match(EBitString t, EMatchContext r) {
			if (free) {
				r.vars.put(var_name, t);
				return true;
			} else {
				return t.equalsExactly(r.vars.get(var_name));
			}
		}
	}

	/**
	 * @param spec 
	 * @param array
	 */
	private EMatchSpec(MatchFunction[] funs, ESeq spec) {
		this.funs = funs;
		this.spec = spec;
	}
	
	@Override
	public String toString() {
		return spec.toString();
	}

	/**
	 * Parse an erlang match_spec, return compiled spec.
	 * 
	 * @param spec
	 * @return compiled/parsed match_spec
	 */
	public static EMatchSpec compile(ESeq spec) {

		ESeq full_spec = spec;
		List<MatchFunction> funs = new ArrayList<MatchFunction>();

		for (; !spec.isNil(); spec = spec.tail()) {
			funs.add(parse_MatchFunction(spec.head()));
		}

		return new EMatchSpec(funs.toArray(new MatchFunction[funs.size()]), full_spec);
	}

	static EObject KEY_ABSENT = new EKeyAbsent();

	/**
	 * If the match head is a tuple, and the element at keypos is a constant
	 * term, then return that term. This is used if there is an index on that
	 * position.
	 * 
	 * @param keypos1
	 * @return
	 */
	public EObject getTupleKey(int keypos1) {
		if (this.funs.length == 1) {
			if (funs[0].head instanceof TuplePattern) {
				TuplePattern tp = (TuplePattern) funs[0].head;
				if (tp.elems.length < keypos1) return KEY_ABSENT;
				if (tp.elems[keypos1 - 1] instanceof EqualsPattern) {
					EqualsPattern ep = (EqualsPattern) tp.elems[keypos1 - 1];
					return ep.value;
				}
			} else if (funs[0].head instanceof EqualsPattern) {
				EqualsPattern ep = (EqualsPattern) funs[0].head;
				ETuple tup;
				if ((tup=ep.value.testTuple()) != null && tup.arity() >= keypos1) {
					return tup.elm(keypos1);
				}
			}
		}

		return null;
	}

	/**
	 * @param res
	 * @param map
	 * @return
	 */
	public ESeq matching_keys(ESeq values, Map<EObject, ETuple> map) {

		for (Map.Entry<EObject, ETuple> ent : map.entrySet()) {

			ETuple val = ent.getValue();
			if (matches(val)) {
				values = values.cons(ent.getKey());
			}
		}

		return values;
	}

	/**
	 * @param candidate
	 * @return
	 */
	public boolean matches(EObject candidate) {
		return match(candidate) == ERT.TRUE;
	}

	public EObject match(EObject candidate) {
		for (int i = 0; i < funs.length; i++) {
			EObject val = funs[i].match(candidate);
			if (val != null) {
				return val;
			}
		}

		return null;
	}

	/**
	 * @param vals
	 * @param map
	 * @return
	 */
	public ESeq matching_values_bag(ESeq vals,
			Map<EObject, IPersistentCollection> map) {
		
		for (Map.Entry<EObject, IPersistentCollection> ent : map.entrySet()) {

			IPersistentCollection values = ent.getValue();
			for (ISeq seq = values.seq(); seq != null; seq = seq.next()) {
				ETuple val = (ETuple) seq.first();
				if (matches(val)) {
					vals = vals.cons(val);
				}
			}
		}

		return vals;
	}

	public ESeq matching_values_coll(ESeq vals, ISeq seq) {
	
		for (; seq != null && !seq.equals(seq.empty()); seq = seq.next()) {
			EObject val = (EObject) seq.first();
			if (matches(val)) {
				vals = vals.cons(val);
			}
		}
		return vals;
	}

	@Override
	public int hashCode() {
		return spec.hashCode();
	}
}

class EKeyAbsent extends EPseudoTerm {
// 	@Override
	int compare_same(EObject rhs) {
		if (rhs == EMatchSpec.KEY_ABSENT)
			return 0;
		return -1;
	}

	@Override
	public int hashCode() {
		return 1;
	}
}
