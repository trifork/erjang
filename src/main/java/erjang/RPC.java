package erjang;

import erjang.m.rpc.MBox;

public class RPC {

	static EAtom am_$gen_call = EAtom.intern("$gen_call");
	static EAtom am_rex = EAtom.intern("rex");
	static EAtom am_undefined = EAtom.intern("undefined");
	static EAtom am_user = EAtom.intern("user");
	static EAtom am_call = EAtom.intern("call");
	static EAtom am_rpc = EAtom.intern("rpc");
	static EAtom am_call_from_java = EAtom.intern("call_from_java");
	static EAtom am_init = EAtom.intern("init");
	static EAtom am_get_status = EAtom.intern("get_status");
	static EAtom am_started = EAtom.intern("started");
	
	
	public static EObject call(EAtom mod, EAtom fun, EObject... args) {
		return call(mod, fun, EList.fromArray(args));
	}
	
	public static EObject call(EAtom mod, EAtom fun, ESeq args) {
		
		MBox mbox = new MBox();
		ESeq callargs = EList.make(mod, fun, args, mbox);
		EProc proc = new EProc(default_groupleader(), am_rpc, am_call_from_java, callargs);
		ERT.run(proc);
		//proc.joinb();

		return mbox.get_b();
	}
	
	private static EPID default_groupleader() {
		return erjang.m.rpc.Native.get_local_group_leader();	
	}
	
	
	public static EObject call(EAtom mod, EAtom fun, ESeq args, long timeout) {
		
		MBox mbox = new MBox();
		
		EProc proc = new EProc(default_groupleader(), 
				               am_rpc, am_call_from_java, 
				               EList.make(mod, fun, args, mbox));
		ERT.run(proc);

		return mbox.get_b(timeout);
	}

	public static void wait_for_erjang_started(long timeout) {
		erjang.m.rpc.Native.wait_for_started(timeout);
	}
	
	
}
