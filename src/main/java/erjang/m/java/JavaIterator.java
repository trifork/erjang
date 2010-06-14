package erjang.m.java;

import java.util.Iterator;

import erjang.EList;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;

/** Wrap an java.util.Iterator and present it as an ESeq */
class JavaIterator extends ESeq {
	Iterator<?> rest;
	Object head;

	static ESeq box(Iterator<?> it) {
		if (it.hasNext())
			return new JavaIterator(it);
		return ERT.NIL;
	}

	private JavaIterator(Iterator<?> it) {
		this.head = it.next();
		this.rest = it;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		return box(rest);
	}

	@Override
	public EObject head() {
		return JavaObject.box(head);
	}
}

