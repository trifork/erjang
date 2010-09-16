package erjang.epmd;

public interface Connection {

	int CLOSED = 0;
	int OPEN = 1;
	int OPENING = 2;
	int CLOSING = 4;

	void setName(String nm);

}
