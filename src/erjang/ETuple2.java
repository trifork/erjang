package org.erlang;

public class ETuple2 extends ETuple {
	public EObject elem1;
	public EObject elem2;

	@Override
	public ETuple2 blank() {
		ETuple2 res = new ETuple2();
		return res;
	}

	@Override
	public int arity() {
		return 2;
	}

	@Override
	public EObject nth(int i) {
		if (i == 1) {
			return elem1;
		} else if (i == 2) {
			return elem2;
		} else {
			return bad_nth(i);
		}
	}

	@Override
	public void set(int i, EObject term) {
		if (i == 1) {
			elem1 = term;
		} else if (i == 2) {
			elem2 = term;
		} else {
			bad_nth(i);
		}
	}
}
