package erjang.m.java;

import java.lang.reflect.Array;

import erjang.EList;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;

/** Wrap an array and present it as an ESeq */
class JavaArray extends ESeq {

	private final int idx;
	private final Object arr;
	private EProc self;

	static ESeq box(EProc self, Object arr, int idx) {
		if (Array.getLength(arr) == idx)
			return ERT.NIL;
		return new JavaArray(self, arr, idx);
	}

	private JavaArray(EProc self, Object arr, int idx) {
		this.arr = arr;
		this.idx = idx;
		this.self = self;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		return box(self, arr, idx + 1);
	}

	@Override
	public EObject head() {
		return JavaObject.box(self, Array.get(arr, idx));
	}
}

