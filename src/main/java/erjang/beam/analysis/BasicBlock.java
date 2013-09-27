/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2009 by Trifork
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *  
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 **/

package erjang.beam.analysis;

import java.util.BitSet;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

class BasicBlock {

	final int label;
	final int index;

	BitSet use = new BitSet();
	BitSet def = new BitSet();
	BitSet in = new BitSet();
	BitSet out = new BitSet();
	
	Set<BasicBlock> succ = new TreeSet<BasicBlock>(
			new Comparator<BasicBlock>() {
				@Override
				public int compare(BasicBlock o1, BasicBlock o2) {
					if (o1 == o2)
						return 0;

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

	public void use_x(int reg) {
		use.set(KEY_X | reg);
	}

	public void use_y(TypeMap map, int reg) {
		use.set(KEY_X | map.get_ypos(reg));
	}

	public void use_fr(int reg) {
		use.set(KEY_X | reg);
	}

	public void kill_x(int reg) {
		def.set(KEY_X | reg);
	}

	public void kill_y(TypeMap map, int reg) {
		def.set(KEY_X | map.get_ypos(reg));
	}

	public void kill_fr(int reg) {
		def.set(KEY_X | reg);
	}

	static final int KEY_X = 0 << 16;
	static final int KEY_Y = 1 << 16;
	static final int KEY_FR = 2 << 16;

}
