package org.erlang;

public class ETuple1 extends ETuple {
	public EObject elem1;
	
	@Override
	public ETuple1 blank() {
		ETuple1 res = new ETuple1();
		return res;
	}

	@Override
	public int arity() {
		return 1;
	}

	@Override
	public void set(int index, EObject term) {
		if (index==1) {
			elem1 = term;
			return;
		}
		bad_nth(index);
	}

	@Override
	public EObject nth(int i) {
		if(i==1) return elem1; 
		return bad_nth(i);
	}

}
