package erjang.m.java;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import erjang.ECons;
import erjang.EList;
import erjang.ENil;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple2;

class JavaMapIterator extends ESeq {
	final Iterator<Entry> rest;
	final Entry<?, ?> ent;
	private EProc self;

	@Override
	public ECons testNonEmptyList() {
		return this;
	}
	
	@Override
	public boolean isNil() {
		return false;
	}
	
	@Override
	public ENil testNil() {
		return null;
	}
	
	@SuppressWarnings("unchecked")
	static ESeq box(EProc self, Iterator<Map.Entry> iterator) {
		if (iterator.hasNext())
			return new JavaMapIterator(self, iterator);
		return ERT.NIL;
	}

	@SuppressWarnings("unchecked")
	private JavaMapIterator(EProc self, Iterator<Map.Entry> it) {
		ent = it.next();
		this.rest = it;
		this.self = self;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		ESeq tail = box(self, rest);
		return tail;
	}

	@Override
	public EObject head() {
		return new ETuple2(JavaObject.box(self, ent.getKey()), 
						   JavaObject.box(self, ent.getValue()));
	}

	@Override
	public int hashCode() {
		return rest.hashCode(); // What to do here? The iterator may be an infinite stream.
	}
}

