package erjang.m.java;

import erjang.ECons;
import erjang.EList;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ESmall;
import erjang.EString;
import erjang.EStringList;

/** Wrap an java.lang.CharSequence and present it as an ESeq */
class JavaCharSeq extends ESeq {
	private final CharSequence seq;
	private final int pos;
	private EProc self;

	public ECons testNonEmptyList() {
		if (seq.length() == pos)
			return null;
		return this;
	}
	
	@Override
	public EString testString() {
		return EString.make(seq, pos, seq.length());
	}

	static ESeq box(EProc self, CharSequence cs, int pos) {
		if (cs.length() == pos)
			return ERT.NIL;
		return new JavaCharSeq(self, cs, pos);
	}

	private JavaCharSeq(EProc self, CharSequence cs, int pos) {
		this.seq = cs;
		this.pos = pos;
		this.self = self;
	}

	@Override
	public ESeq cons(EObject h) {
		ESmall s;
		if ((s = h.testSmall()) != null) {
			if ((s.value & 0xff) == s.value) {
				return new EStringList((byte) s.value, this);
			}
		}
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		return box(self, seq, pos + 1);
	}

	@Override
	public EObject head() {
		return ERT.box(seq.charAt(pos));
	}

	@Override
	public int hashCode() {
		return seq.hashCode();
	}
	//TODO: equality operators
}
