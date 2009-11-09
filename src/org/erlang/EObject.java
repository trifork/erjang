package org.erlang;

public class EObject {

	public ECons cons(EObject h)
	{
		return new EPair(h, this);
	}
	

	public EAtom asAtom() {
		return null;
	}

	public ETuple asTuple() {
		return null;
	}

	public ESeq asSeq() {
		return null;
	}

	public EPID asPID() {
		return null;
	}

	public int asInt() {
		throw new RuntimeException("cannot convert " + this + " to int");
	}

	public ENumber asNumber() {
		return null;
	}


	public ECons asCons() {
		return null;
	}

}
