package erjang.m.erlang;

public interface EDist {
	public static final int DFLAG_PUBLISHED           =0x01;
	public static final int DFLAG_ATOM_CACHE          =0x02;
	public static final int DFLAG_EXTENDED_REFERENCES =0x04;
	public static final int DFLAG_DIST_MONITOR        =0x08;
	public static final int DFLAG_FUN_TAGS            =0x10;
	public static final int DFLAG_DIST_MONITOR_NAME   =0x20;
	public static final int DFLAG_HIDDEN_ATOM_CACHE   =0x40;
	public static final int DFLAG_NEW_FUN_TAGS        =0x80;
	public static final int DFLAG_EXTENDED_PIDS_PORTS =0x100;
	public static final int DFLAG_EXPORT_PTR_TAG      =0x200;
	public static final int DFLAG_BIT_BINARIES        =0x400;
	public static final int DFLAG_NEW_FLOATS          =0x800;
	public static final int DFLAG_UNICODE_IO          =0x1000;
	public static final int DFLAG_DIST_HDR_ATOM_CACHE =0x2000;
	public static final int DFLAG_SMALL_ATOM_TAGS     =0x4000;

	/* All flags that should be enabled when term_to_binary/1 is used. */
	public static final int TERM_TO_BINARY_DFLAGS = (DFLAG_EXTENDED_REFERENCES	
				       | DFLAG_NEW_FUN_TAGS		
				       | DFLAG_EXTENDED_PIDS_PORTS	
				       | DFLAG_EXPORT_PTR_TAG		
				       | DFLAG_BIT_BINARIES);

	/* opcodes used in distribution messages */
	public static final int DOP_LINK		=1;
	public static final int DOP_SEND		=2;
	public static final int DOP_EXIT		=3;
	public static final int DOP_UNLINK		=4;
	public static final int DOP_NODE_LINK		=5;
	public static final int DOP_REG_SEND		=6;
	public static final int DOP_GROUP_LEADER	=7;
	public static final int DOP_EXIT2		=8;

	public static final int DOP_SEND_TT		=12;
	public static final int DOP_EXIT_TT		=13;
	public static final int DOP_REG_SEND_TT		=16;
	public static final int DOP_EXIT2_TT		=18;

	public static final int DOP_MONITOR_P		=19;
	public static final int DOP_DEMONITOR_P		=20;
	public static final int DOP_MONITOR_P_EXIT	=21;

}
