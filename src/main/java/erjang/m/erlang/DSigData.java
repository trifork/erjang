package erjang.m.erlang;

import java.util.Map;

import erjang.EAtom;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EProc;
import erjang.ERT;

public class DSigData implements EDist {

	private static final int DFLAG_DIST_HDR_ATOM_CACHE = 0;
	
	DistEntry dep;
	EProc proc;
	private boolean no_suspend;

	public int send(EObject ctl, EObject msg, boolean force_busy) {
		EInternalPort cid;
		int suspended = 0;
		int resume = 0;
		int pass_through_size;
		int data_size, dhdr_ext_size;
		DistEntry dep = this.dep;
		int flags = dep.flags;
		EProc c_p = this.proc;
		Map<Integer,EAtom> acmp;

		if (c_p == null || this.no_suspend)
			force_busy = true;

		if (ErlDist.erts_is_alive != ERT.TRUE) {
			return 0;
		}

		if ((flags & DFLAG_DIST_HDR_ATOM_CACHE) != 0) {
			acmp = c_p.getAtomCacheMap();
			pass_through_size = 0;
		} else {
			acmp = null;
			pass_through_size = 1;
		}

		data_size = pass_through_size;
		acmp.clear(); // erts_reset_atom_cache_map
		
		data_size += 0;
		
		// TODO: not done!
		return 0;
	}
}
