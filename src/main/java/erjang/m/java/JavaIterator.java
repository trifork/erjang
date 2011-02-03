package erjang.m.java;

import java.util.Iterator;

import erjang.EList;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;

/** Wrap an java.util.Iterator and present it as an ESeq */
class JavaIterator extends ESeq {
	Iterator<?> rest;
	Object head;
	private EProc self;

	static ESeq box(EProc self, Iterator<?> it) {
		if (it.hasNext())
			return new JavaIterator(self, it);
		return ERT.NIL;
	}

	private JavaIterator(EProc self, Iterator<?> it) {
		this.head = it.next();
		this.rest = it;
		this.self = self;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		return box(self, rest);
	}

	@Override
	public EObject head() {
		return JavaObject.box(self, head);
	}

	@Override
	public int hashCode() {
		return head.hashCode() + rest.hashCode(); // What to do here? The iterator may be an infinite stream.
	}
}

