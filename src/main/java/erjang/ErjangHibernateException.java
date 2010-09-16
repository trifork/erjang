package erjang;

public class ErjangHibernateException extends RuntimeException {
	
	public static final ErjangHibernateException INSTANCE = new ErjangHibernateException();
	private ErjangHibernateException() {
	}
}
