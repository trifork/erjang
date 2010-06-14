package erjang.m.java;

import java.lang.reflect.Array;

import erjang.EList;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;

/** Wrap an array and present it as an ESeq */
class JavaArray extends ESeq {

	private final int idx;
	private final Object arr;

	static ESeq box(Object arr, int idx) {
		if (Array.getLength(arr) == idx)
			return ERT.NIL;
		return new JavaArray(arr, idx);
	}

	private JavaArray(Object arr, int idx) {
		this.arr = arr;
		this.idx = idx;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		return box(arr, idx + 1);
	}

	@Override
	public EObject head() {
		return JavaObject.box(Array.get(arr, idx));
	}
}

