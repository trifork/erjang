package org.erlang;

public abstract class ECons extends ETerm {

	public static final ENil EMPTY = new ENil();

	public abstract EObject head();
	public abstract EObject tail();
	
	public ECons asCons() {
		return this;
	}

	// generic count function that also works for non-well-formed lists
	public int count() {
		EObject cell = tail();
		int count = 1;
		while (cell != EMPTY && (cell instanceof ECons)) {
			cell = ((ECons)cell).tail();
			count += 1;
		}
		return count;
	}

}

