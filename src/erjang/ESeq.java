package org.erlang;

import java.util.ArrayList;
import java.util.List;


public abstract class ESeq extends ECons {

	public ESeq asSeq() {
		return this;
	}

	@Override
	public abstract ESeq tail();

	public abstract ESeq cons(EObject h);

	public EObject[] toArray() {
		List<EObject> out = new ArrayList<EObject>();
		ESeq curr = this;
		while (curr != EMPTY) {
			out.add(curr.head());
			curr = curr.tail();
		}
		return out.toArray(new EObject[out.size()]);
	}

	public int length() {
		int count = 0;
		ESeq curr = this;
		while (curr != EMPTY) {
			count += 1;
			curr = curr.tail();
		}
		return count;
	}

}
