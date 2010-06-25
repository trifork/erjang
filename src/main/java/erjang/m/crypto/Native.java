package erjang.m.crypto;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

import erjang.BIF;
import erjang.EBinary;
import erjang.ENative;
import erjang.EObject;
import erjang.ERT;

public class Native extends ENative {

	int get_int32(EBinary bin) {
		return bin.intBitsAt(0, 32);
	}
	
	static SecureRandom rand;
	
	static {
		try {
			rand = SecureRandom.getInstance("SHA1PRNG");
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}
	}
	
	@BIF
	public static EObject sha(EObject data) 
	{
		EBinary bin = data.testBinary();
		if (bin == null) 
			throw ERT.badarg(data);
		
		MessageDigest sha;
		try {
			sha = MessageDigest.getInstance("SHA");
		} catch (NoSuchAlgorithmException e) {
			throw ERT.badarg(data);
		}

		sha.digest(bin.getByteArray());
		
		byte[] res = sha.digest();
		
		return EBinary.make(res);
	}
	
	@BIF
	public static EObject md5(EObject data) 
	{
		EBinary bin = data.testBinary();
		if (bin == null) 
			throw ERT.badarg(data);
		
		MessageDigest md5;
		try {
			md5 = MessageDigest.getInstance("MD5");
		} catch (NoSuchAlgorithmException e) {
			throw ERT.badarg(data);
		}

		md5.digest(bin.getByteArray());
		
		byte[] res = md5.digest();
		
		return EBinary.make(res);
	}
	
	@BIF
	public static EObject rand_uniform_nif(EObject from, EObject to)
	{
		EBinary fb = from.testBinary();
		EBinary tb = to.testBinary();
		
		if (fb == null || tb == null) {
			throw ERT.badarg(from, to);
		}
		
		BigInteger fi = mp2big(fb);
		BigInteger ti = mp2big(tb);
		
		System.err.println("rand_uniform ("+fb+", "+tb+")");
		
		BigInteger interval = ti.subtract(fi).subtract(BigInteger.ONE);
		
		BigInteger base_value = new BigInteger(interval.bitLength(), rand);
		
		BigInteger result;
		while (interval.compareTo(base_value) == 1) {
			base_value = new BigInteger(interval.bitLength(), rand);
		}

		result = fi.add(base_value);
		
		System.err.println("rand_uniform ("+fi+", "+ti+") -> "+result);

		EBinary res = big2mp(result);
		return res;
	}

	private static EBinary big2mp(BigInteger result) {
		byte[] bytes = result.toByteArray();
		
		ByteBuffer out = ByteBuffer.allocate(4 + bytes.length);
		out.putInt(bytes.length);
		out.put(bytes);
		out.position(0);
		
		EBinary res = EBinary.make(out);
		return res;
	}

	private static BigInteger mp2big(EBinary bin) {
		ByteBuffer bb = bin.toByteBuffer();
		int len = bb.getInt();
		byte[] data = new byte[len];
		bb.get(data);
		return new BigInteger(data);
	}
	
	
}
