package org.erlang;

import java.lang.annotation.ElementType;
import java.lang.annotation.RetentionPolicy;

@java.lang.annotation.Retention(RetentionPolicy.RUNTIME)
@java.lang.annotation.Target(ElementType.METHOD)
public @interface bif {

	/** name */
	String name() default "__SELF__";
	
	/** is this a guard bif? */
	boolean guard() default false;

	/** module this bif belongs to */
	String module() default "erlang";
	
	/** export from module */
	boolean export() default false;
	
}
