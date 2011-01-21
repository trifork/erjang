package erjang.m.binary;

import erjang.*;
import erjang.m.erlang.ErlBif;
import erjang.m.erlang.ErlConvert;

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
 * TODO: use ETupleN instead of ETuple (specific classes)
 * TODO: bin_to_list:/* C code works with big binaries in several iterations
 * (callback starting with a calculated max. loop count). How should we do that?
 * TODO: can binaries become awkwardly big so that EString.make duplicating the byte array would become a problem?
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

    protected static EString do_bin_to_list(EObject subject, EObject pos, EObject len) {
        if (subject == null) throw ERT.badarg(subject);

        EBinary bin = subject.testBinary();
        if (bin == null) throw ERT.badarg(subject);
        if (!bin.isBinary()) throw ERT.badarg(subject);

        if (pos.testSmall() == null) throw ERT.badarg(pos);
        if (len.testSmall() == null) throw ERT.badarg(len);

        //TODO: unclear: C code operates with bit_offs which seems to be always 0 for binary:* calls
        //implement using EString for now, but try to find a case where bit_offs actually is set to != 0
        //so the EString implementation won't work (hypothesis)
        //TODO: another question: C code uses callbacks for big binaries. Here, we don't. This aspect needs to be
        //closer evaluated

        //we have to do some dirty hacks with indexes to fool the ErlConverter thinking we start with 1 and
        //expecting the stop instead of length, calculating length itself
        ESmall start = new ESmall(pos.asInt() + 1);
        ESmall stop = new ESmall((len.asInt() == -1) ? bin.byteSize() : len.asInt() + pos.asInt());

        return ErlConvert.binary_to_list(subject, start, stop);
    }

	/**
	 * bin_to_list(Subject) -> list()
	 */
	@BIF
	public static EString bin_to_list(EObject subject) {
        return do_bin_to_list(subject, new ESmall(0), new ESmall(-1));
	}

	/**
	 * bin_to_list(Subject, PosLen) -> list()
	 */
	@BIF
	public static EString bin_to_list(EObject subject, EObject poslen) {
        if (poslen == null) throw ERT.badarg(poslen);

        ETuple tuple = poslen.testTuple();
        if (tuple == null) throw ERT.badarg(poslen);

        ETuple2 tuple2 = ETuple2.cast(tuple);
        if (tuple2 == null) throw ERT.badarg(poslen);

        if (tuple2.elem1.testSmall() == null) throw ERT.badarg(tuple2.elem1);
        if (tuple2.elem2.testSmall() == null) throw ERT.badarg(tuple2.elem2);

		return do_bin_to_list(subject, new ESmall(tuple2.elem1.asInt()), new ESmall(tuple2.elem2.asInt()));
	}
	
	/**
	 * bin_to_list(Subject, Pos, Len) -> list()
	 */
	@BIF
	public static EString bin_to_list(EObject subject, EObject pos, EObject len) {
		return do_bin_to_list(subject, pos, len);
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
        return ErlBif.list_to_binary(byteList);
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
}
