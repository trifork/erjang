package org.erlang;

public class ETuple4 extends ETuple {

	public EObject elem1;
	public EObject elem2;
	public EObject elem3;
	public EObject elem4;

	@Override
	public ETuple4 blank() {
		ETuple4 res = new ETuple4();
		return res;
	}

	@Override
	public int arity() {
		return 4;
	}

	@Override
	public EObject nth(int i) {
		switch (i) {
		case 1:
			return elem1;
		case 2:
			return elem2;
		case 3:
			return elem3;
		case 4:
			return elem4;
		default:
			return bad_nth(i);
		}
	}

	@Override
	public void set(int i, EObject term) {
		switch (i) {
		case 1:
			elem1 = term;
			break;
		case 2:
			elem2 = term;
			break;
		case 3:
			elem3 = term;
			break;
		case 4:
			elem4 = term;
			break;
		default:
			bad_nth(i);
		}
	}
}
