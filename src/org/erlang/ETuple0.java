package org.erlang;

public class ETuple0 extends ETuple {
	@Override
	public ETuple0 blank() {
		return new ETuple0();
	}
	
	@Override
	public int arity() {
		return 0;
	}

	@Override
	public EObject nth(int i) {
		return bad_nth(i);
	}
	
	@Override
	public void set(int index, EObject term) {
		bad_nth(index);
	}
}
