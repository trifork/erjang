package erjang.m.binary;

import erjang.BIF;
import erjang.EBinary;
import erjang.EInteger;
import erjang.EList;
import erjang.ENative;
import erjang.EObject;
import erjang.ETuple;
import erjang.NotImplemented;

/**
 * The implementation of Erlang's binary module
 * 
 * @author Pavlo Baron (pb@pbit.org)
 * 
 * TODO: port implementation (as far as possible) from erts/emulator/beam/erl_bif_binary.c
 * TODO: extend signature with EProc where necessary (acc. to the corresponding C code)
 * TODO: take care of correct usage of EBig and ESmall. Don't use EInteger
 * TODO: no EUnsigned class, instead checks for BigInteger.signum where necessary
 * TODO: implement an EPart class locally?
 *
 */
public class Native extends ENative {

	/**
	 * at(Subject, Pos) -> int()
	 */
	@BIF
	public static EInteger at(EObject subject, EObject pos) {
		throw new NotImplemented();
	}
	
	/**
	 * bin_to_list(Subject) -> list()
	 */
	@BIF
	public static EList bin_to_list(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * bin_to_list(Subject, PosLen) -> list()
	 */
	@BIF
	public static EList bin_to_list(EObject subject, EObject poslen) {
		throw new NotImplemented();
	}
	
	/**
	 * bin_to_list(Subject, Pos, Len) -> list()
	 */
	@BIF
	public static EList bin_to_list(EObject subject, EObject pos, EObject len) {
		throw new NotImplemented();
	}
	
	/**
	 * compile_pattern(Pattern) -> cp()
	 */
	@BIF
	public static ETuple compile_pattern(EObject pattern) {
		throw new NotImplemented();
	}
	
	/**
	 * copy(Subject) -> binary()
	 */
	@BIF
	public static EBinary copy(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * copy(Subject,N) -> binary()
	 */
	@BIF
	public static EBinary copy(EObject subject, EObject n) {
		throw new NotImplemented();
	}
	
	/**
	 * decode_unsigned(Subject) -> Unsigned
	 */
	@BIF
	public static EInteger decode_unsigned(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * decode_unsigned(Subject, Endianess) -> Unsigned
	 */
	@BIF
	public static EInteger decode_unsigned(EObject subject, EObject endianess) {
		throw new NotImplemented();
	}
	
	/**
	 * encode_unsigned(Unsigned) -> binary()
	 */
	@BIF
	public static EBinary encode_unsigned(EObject unsigned) {
		throw new NotImplemented();
	}
	
	/**
	 * encode_unsigned(Unsigned,Endianess) -> binary()
	 */
	@BIF
	public static EBinary encode_unsigned(EObject unsigned, EObject endianess) {
		throw new NotImplemented();
	}
	
	/**
	 * first(Subject) -> int()
	 */
	@BIF
	public static EInteger first(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * last(Subject) -> int()
	 */
	@BIF
	public static EInteger last(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * list_to_bin(ByteList) -> binary()
	 */
	@BIF
	public static EBinary list_to_bin(EObject byteList) {
		throw new NotImplemented();
	}
	
	/**
	 * longest_common_prefix(Binaries) -> int()
	 */
	@BIF
	public static EInteger longest_common_prefix(EObject binaries) {
		throw new NotImplemented();
	}
	
	/**
	 * longest_common_suffix(Binaries) -> int()
	 */
	@BIF
	public static EInteger longest_common_suffix(EObject binaries) {
		throw new NotImplemented();
	}
	
	/**
	 * match(Subject, Pattern) -> Found | nomatch
	 */
	@BIF
	public static EObject match(EObject subject, EObject pattern) {
		throw new NotImplemented();
	}
	
	/**
	 * match(Subject,Pattern,Options) -> Found | nomatch
	 */
	@BIF
	public static EObject match(EObject subject, EObject pattern, EObject options) {
		throw new NotImplemented();
	}
	
	/**
	 * matches(Subject, Pattern) -> Found
	 */
	@BIF
	public static EObject matches(EObject subject, EObject pattern) {
		throw new NotImplemented();
	}
	
	/**
	 * matches(Subject,Pattern,Options) -> Found
	 */
	@BIF
	public static EObject matches(EObject subject, EObject pattern, EObject options) {
		throw new NotImplemented();
	}
	
	/**
	 * part(Subject, PosLen) -> binary()
	 */
	@BIF
	public static EBinary part(EObject subject, EObject poslen) {
		throw new NotImplemented();
	}
	
	/**
	 * part(Subject, Pos, Len) -> binary()
	 */
	@BIF
	public static EBinary part(EObject subject, EObject pos, EObject len) {
		throw new NotImplemented();
	}
	
	/**
	 * referenced_byte_size(binary()) -> int()
	 */
	@BIF
	public static EInteger referenced_byte_size(EObject subject) {
		throw new NotImplemented();
	}
	
	/**
	 * replace(Subject,Pattern,Replacement) -> Result
	 */
	@BIF
	public static EBinary replace(EObject subject, EObject pattern, EObject replacement) {
		throw new NotImplemented();
	}
	
	/**
	 * replace(Subject,Pattern,Replacement,Options) -> Result
	 */
	@BIF
	public static EBinary replace(EObject subject, EObject pattern, EObject replacement, EObject options) {
		throw new NotImplemented();
	}
		
	/**
	 * split(Subject,Pattern) -> Parts
	 */
	@BIF
	public static EList split(EObject subject, EObject pattern) {
		throw new NotImplemented();
	}
	
	/**
	 * split(Subject,Pattern,Options) -> Parts
	 */
	@BIF
	public static EList split(EObject subject, EObject pattern, EObject options) {
		throw new NotImplemented();
	}
}
