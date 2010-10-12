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

package erjang.m.ets;

import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentCollection;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.PersistentList;
import com.trifork.clj_ds.PersistentTreeMap;

/**
 * 
 */
public class PersistentBag extends Object implements IPersistentBag {

	public static PersistentBag EMPTY = new PersistentBag(PersistentHashMap.EMPTY, 0);
	IPersistentMap impl;
	int count;

	private PersistentBag(IPersistentMap impl, int count) {
		this.impl = impl;
		this.count = count;
	}

	@Override
	public IPersistentBag disjoin(Object element) {
		Integer val = (Integer) impl.valAt(element);
		if (val == null) {
			return this;
		} else if (val == 1) {
			try {
				return new PersistentBag(impl.without(element), count - 1);
			} catch (Exception e) {
				throw new Error(e);
			}
		} else {
			return new PersistentBag(impl.assoc(element, val - 1), count - 1);
		}
	}

	@Override
	public IPersistentCollection cons(Object element) {
		Integer val = (Integer) impl.valAt(element);
		if (val == null) {
			return new PersistentBag(impl.assoc(element, 1), count + 1);
		} else {
			return new PersistentBag(impl.assoc(element, val + 1), count + 1);
		}
	}

	@Override
	public int count() {
		return count;
	}

	@Override
	public IPersistentCollection empty() {
		return new PersistentBag(PersistentTreeMap.EMPTY, 0);
	}

	@Override
	public boolean equiv(Object arg0) {
		throw new UnsupportedOperationException();
	}

	class ElemSeq implements ISeq {

		private final int total;
		private final int pos;
		private final ISeq pairs;

		@Override
		public ISeq cons(Object arg0) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public Object first() {
			IMapEntry elem_count_pair = (IMapEntry) pairs.first();
			return elem_count_pair.getKey();
		}

		@Override
		public ISeq more() {
			ISeq next = next();
			if (next == null)
				return PersistentList.EMPTY;
			return next;
		}

		@Override
		public ISeq next() {
			IMapEntry elem_count_pair = (IMapEntry) pairs.first();
			int count = (Integer) elem_count_pair.getValue();
			if (count == pos + 1) {
				ISeq succ = pairs.next();
				if (succ == null) {
					return null;
				} else {
					return new ElemSeq(succ, 0, total-1);
				}
			} else {
				return new ElemSeq(pairs, pos + 1, total-1);
			}
		}

		@Override
		public int count() {
			return total;
		}

		@Override
		public IPersistentCollection empty() {
			return PersistentBag.EMPTY;
		}

		@Override
		public boolean equiv(Object arg0) {
			throw new UnsupportedOperationException();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see com.trifork.clj_ds.Seqable#seq()
		 */
		@Override
		public ISeq seq() {
			return this;
		}

		ElemSeq(ISeq pairs, int count, int total) {
			this.pairs = pairs;
			this.pos = count;
			this.total = total;
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.trifork.clj_ds.Seqable#seq()
	 */
	@Override
	public ISeq seq() {
		if (count() == 0) return null;
		return new ElemSeq(impl.seq(), 0, count);
	}

}
