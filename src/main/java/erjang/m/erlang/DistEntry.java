package erjang.m.erlang;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import erjang.EAtom;
import erjang.EInternalPort;

public class DistEntry {

	static Map<EAtom, DistEntry> table = new ConcurrentHashMap<EAtom, DistEntry>();
	
	static {
		
	}
	
	private final EAtom sysname;
	private final EInternalPort cid;
	public int flags;

	
	
	public DistEntry(EAtom sysname, EInternalPort cid) {
		this.sysname = sysname;
		this.cid = cid;

		table.put(sysname, this);
	}

	/** get DistEntry for sysname (may return null) */
	static DistEntry find(EAtom sysname) {
		return table.get(sysname);
	}
	
	static DistEntry find_or_insert(EAtom sysname) {
		DistEntry ent = table.get(sysname);
		if (ent == null) {
			return new DistEntry(sysname, null);
		} else {
			return ent;
		}
	}
	
	static DistEntry sysname_to_connected_dist_entry(EAtom sysname) {
		DistEntry ent = table.get(sysname);
		if (ent == null) {
			return null;
		} else {
			return ent.cid == null ? null : ent;
		}

	}
	
	
}
