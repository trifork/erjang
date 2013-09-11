package erjang.m.string;

import java.math.BigInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import erjang.BIF;
import erjang.EAtom;
import erjang.EDouble;
import erjang.EInteger;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETuple2;

public class Native extends ENative {

	public static EAtom am_no_float = EAtom.intern("no_float");
	public static EAtom am_no_integer = EAtom.intern("no_integer");
	public static EAtom am_error = EAtom.intern("error");
	
	static Pattern int_pattern = Pattern.compile("^((-|\\+)?(\\d+)).*");
	static Pattern float_pattern = Pattern.compile("^((-|\\+)?\\d*\\.\\d+([eE](-|\\+)?\\d+)?).*");
	
	@BIF
	public static ETuple2 to_integer(EObject val)
	{
		EString v = val.testString();
		if (v == null) throw ERT.badarg(val);
		
		String s = v.stringValue();
		Matcher m = int_pattern.matcher(s);
		if (m.matches()) {
			String ss = m.group(1);
			EInteger intval = ERT.box_parse(ss);
			ESeq rest;
			if (ss.length() == s.length()) {
				rest = ERT.NIL; 
			} else {
				rest = new EString(s.substring(ss.length()));
			}
			return new ETuple2(intval, rest);			
		} else {
			return new ETuple2(am_error, am_no_integer);
		}
	}
	
	@BIF
	public static ETuple2 to_float(EObject val)
	{
		EString v = val.testString();
		if (v == null) throw ERT.badarg(val);
		
		String s = v.stringValue();
		Matcher m = float_pattern.matcher(s);
		if (m.matches()) {
			String ss = m.group(1);
			EDouble floatval = ERT.box( Double.parseDouble(ss) );
			ESeq rest;
			if (ss.length() == s.length()) {
				rest = ERT.NIL; 
			} else {
				rest = new EString(s.substring(ss.length()));
			}
			return new ETuple2(floatval, rest);			
		} else {
			return new ETuple2(am_error, am_no_float);
		}
	}
	
}
