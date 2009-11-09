package org.erlang.beam;

import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import org.erlang.ETuple2;

class BasicBlock {

	final int label;
	final int index;

	TreeSet<Integer> use = new TreeSet<Integer>();
	TreeSet<Integer> kill = new TreeSet<Integer>();
	
	TreeSet<Integer> in = new TreeSet<Integer>();
	TreeSet<Integer> out = new TreeSet<Integer>();
	
	Set<BasicBlock> succ = new TreeSet<BasicBlock>(
			new Comparator<BasicBlock>() {
				@Override
				public int compare(BasicBlock o1, BasicBlock o2) {
					if (o1==o2) return 0;
					
					int loff = o1.label - o2.label;
					if (loff != 0) {
						return loff;
					}

					return o1.index - o2.index;
				}
			});;

	public BasicBlock(int label, int index) {
		this.label = label;
		this.index = index;
	}

	public void succ(BasicBlock bb) {
		succ.add(bb);
	}

	public void use_x(int reg) { use.add(KEY_X | reg); }
	public void use_y(TypeMap map, int reg) { use.add(KEY_X | map.get_ypos(reg)); }
	public void use_fr(int reg) { use.add(KEY_X | reg); }

	public void kill_x(int reg) { kill.add(KEY_X | reg); }
	public void kill_y(TypeMap map, int reg) { kill.add(KEY_X | map.get_ypos(reg)); }
	public void kill_fr(int reg) { kill.add(KEY_X | reg); }

	static final int KEY_X = 0 << 16;
	static final int KEY_Y = 1 << 16;
	static final int KEY_FR = 2 << 16;


}		

