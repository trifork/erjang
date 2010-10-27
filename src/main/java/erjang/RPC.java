package erjang;

import erjang.m.erlang.ErlBif;
import erjang.m.erlang.ErlProc;
import erjang.m.rpc.MBox;

public class RPC {

	static EAtom am_$gen_call = EAtom.intern("$gen_call");
	static EAtom am_rex = EAtom.intern("rex");
	static EAtom am_undefined = EAtom.intern("undefined");
	static EAtom am_user = EAtom.intern("user");
	static EAtom am_call = EAtom.intern("call");
	static EAtom am_rpc = EAtom.intern("rpc");
	static EAtom am_call_from_java = EAtom.intern("call_from_java");
	
	
	
	public static EObject call(EAtom mod, EAtom fun, ESeq args) {
		
		MBox mbox = new MBox();
		EProc proc = new EProc(null, am_rpc, am_call_from_java, EList.make(mod, fun, args, mbox));
		ERT.run(proc);
		proc.joinb();

		return mbox.get_b();
	}
	
	
	public static EObject call(EAtom mod, EAtom fun, ESeq args, long timeout) {
		
		MBox mbox = new MBox();
		
		EProc proc = new EProc(null, am_rpc, am_call_from_java, 
							   EList.make(mod, fun, args, mbox));
		ERT.run(proc);

		return mbox.get_b(timeout);
	}
	
	
}
