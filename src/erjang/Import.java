package erjang;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** 
 * In a module, imports are declared using this annotation on static fields of type EFun
 * 
 */

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Import {
	String module();
	String fun();
	int arity();
}
