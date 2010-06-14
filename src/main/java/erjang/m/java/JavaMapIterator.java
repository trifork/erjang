package erjang.m.java;

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import erjang.ECons;
import erjang.EList;
import erjang.ENil;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.ETuple2;

class JavaMapIterator extends ESeq {
	final Iterator<Entry> rest;
	final Entry<?, ?> ent;

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
	static ESeq box(Iterator<Map.Entry> iterator) {
		if (iterator.hasNext())
			return new JavaMapIterator(iterator);
		return ERT.NIL;
	}

	@SuppressWarnings("unchecked")
	private JavaMapIterator(Iterator<Map.Entry> it) {
		ent = it.next();
		this.rest = it;
	}

	@Override
	public ESeq cons(EObject h) {
		return new EList(h, this);
	}

	@Override
	public ESeq tail() {
		ESeq tail = box(rest);
		return tail;
	}

	@Override
	public EObject head() {
		return new ETuple2(JavaObject.box(ent.getKey()), JavaObject.box(ent
				.getValue()));
	}
}

