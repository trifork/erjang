package org.erlang;

import java.lang.annotation.ElementType;
import java.lang.annotation.RetentionPolicy;

@java.lang.annotation.Retention(RetentionPolicy.RUNTIME)
@java.lang.annotation.Target(ElementType.METHOD)
public @interface bif {

	/** name */
	String name() default "__SELF__";

	/** */
	Type type() default Type.DEFAULT;
	
	/** module this bif belongs to */
	String module() default "erlang";
	
	public enum Type {
		DEFAULT, 
		GUARD, 
		ARITHBIF
	}

}
