package erjang.m.rpc;

import kilim.Pausable;
import erjang.BIF;
import erjang.ENative;
import erjang.EObject;
import erjang.EPID;
import erjang.EProc;
import erjang.ERT;
import erjang.ETuple;
import erjang.ErlangException;
import erjang.m.erlang.ErlBif;
import erjang.m.erlang.ErlProc;

public class Native extends ENative {

	static MBox started_mbox = new MBox();
	private static EPID local_group_leader;
	
	@BIF
	static public EObject erjang_started(EProc self) throws Pausable 
	{
		local_group_leader = self.group_leader();
		started_mbox.put(ERT.am_ok);
		return ERT.am_ok;
	}

	public static void wait_for_started(long timeout) {
		started_mbox.get_b(timeout);
	}

	public static EPID get_local_group_leader() {
		return local_group_leader;
	}

	@BIF
	static public EObject call_from_java(EProc self, EObject m, EObject f, EObject args, 
										 EObject mbox) throws Pausable
	
	{
		if (mbox instanceof MBox) {
			MBox embox = (MBox) mbox;
			
			try {
				EObject res = ErlBif.apply(self, m, f, args);
				embox.put(ETuple.make(ERT.am_ok, res));				
				return res;
			} catch (ErlangException e) {
				embox.put(ETuple.make(ERT.am_error, e.getCatchValue()));	
				return ERT.am_undefined;
			}

		} else {
			throw ERT.badarg();
		}
	}
	
}
