package erjang.beam;
import erjang.EAtom;

public class CodeAtoms {
	public static final EAtom TRUE_ATOM = EAtom.intern("true");
	public static final EAtom FALSE_ATOM = EAtom.intern("false");
	public static final EAtom X_ATOM = EAtom.intern("x");
	public static final EAtom Y_ATOM = EAtom.intern("y");
	public static final EAtom FR_ATOM = EAtom.intern("fr");
	public static final EAtom NIL_ATOM = EAtom.intern("nil");
	public static final EAtom INTEGER_ATOM = EAtom.intern("integer");
	public static final EAtom STRING_ATOM = EAtom.intern("string");
	public static final EAtom FLOAT_ATOM = EAtom.intern("float");
	public static final EAtom ATOM_ATOM = EAtom.intern("atom");
	public static final EAtom LIST_ATOM = EAtom.intern("list");
	public static final EAtom LITERAL_ATOM = EAtom.intern("literal");
	public static final EAtom TEST_ATOM = EAtom.intern("test");
	public static final EAtom ALLOC_ATOM = EAtom.intern("alloc");
	public static final EAtom WORDS_ATOM = EAtom.intern("words");
	public static final EAtom FLOATS_ATOM = EAtom.intern("floats");
	public static final EAtom NOFAIL_ATOM = EAtom.intern("nofail");
	public static final EAtom START_ATOM = EAtom.intern("start");
	public static final EAtom F_ATOM = EAtom.intern("f");
	public static final EAtom FIELD_FLAGS_ATOM = EAtom.intern("field_flags");
	public static final EAtom EXTFUNC_ATOM = EAtom.intern("extfunc");
	public static final EAtom APPLY_ATOM = EAtom.intern("apply");
	public static final EAtom ERLANG_ATOM = EAtom.intern("erlang");
	public static final EAtom ERROR_ATOM = EAtom.intern("error");
	public static final EAtom EXIT_ATOM = EAtom.intern("exit");
	public static final EAtom THROW_ATOM = EAtom.intern("throw");
	public static final EAtom BEAM_FILE_ATOM = EAtom.intern("beam_file");
	public static final EAtom FUNCTION_ATOM  = EAtom.intern("function");

	public static final EAtom BIF_ATOM = EAtom.intern("bif");
	public static final EAtom GCBIF_ATOM = EAtom.intern("gc_bif");
	public static final EAtom ARITHFBIF_ATOM = EAtom.intern("arithfbif");
}
