/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2013 by Trifork
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.trifork.clj_ds.APersistentMap;
import com.trifork.clj_ds.APersistentSet;
import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentCollection;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.IPersistentSet;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.PersistentHashSet;
import com.trifork.clj_ds.PersistentTreeMap;
import com.trifork.clj_ds.RT;

public class EPersistentInsertionOrderedSet<V> extends APersistentSet<V> {

	
	public static class EIterator<V> implements Iterator<V> {

		private Iterator<Map.Entry<Long, V>> it;

		public EIterator(
				Iterator<Map.Entry<Long, V>> it) {
					this.it = it;
		}

		@Override
		public boolean hasNext() {
			return it.hasNext();
		}

		@Override
		public V next() {
			return it.next().getValue();
		}

		@Override
		public void remove() {
			it.remove();
		}

	}

	private final IPersistentMap<Long, V> iorder;
	private final IPersistentMap<V, Long> korder;
	private final long ins;
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static EPersistentInsertionOrderedSet EMPTY = new EPersistentInsertionOrderedSet(0, PersistentTreeMap.EMPTY, PersistentHashMap.EMPTY);

	private EPersistentInsertionOrderedSet(long ins, IPersistentMap<Long,V> iorder, IPersistentMap<V,Long> keyorder)
	{
		super(null);
		this.ins = ins+1;
		this.iorder = iorder;
		this.korder = keyorder;		
	}
	

	@Override
	public IPersistentSet<V> disjoin(V value) throws Exception {
		Long idx = korder.valAt(value);
		IPersistentMap<Long, V> i = iorder;
		if (idx == null) { return this; }
		
		i = i.without(idx);
		IPersistentMap<V, Long> k = korder.without(value);
		return make(i, k);	
	}
	
	@Override
	public IPersistentSet<V> cons(V value) {
		Long idx = korder.valAt(value);
		IPersistentMap<Long, V> i = iorder;
		if (idx != null) {
			try {
				i = i.without(idx);
			} catch (Exception e) {
				// ignore //
			}
		}
		Long inso = new Long(ins);
		i = i.assoc(inso, value);
		IPersistentMap<V, Long> k = korder.assoc(value, inso);
		return make(i, k);
	}


	private IPersistentSet<V> make(IPersistentMap<Long, V> i,
			IPersistentMap<V, Long> k) {
		return new EPersistentInsertionOrderedSet<V>(ins, i, k);
	}

	@Override
	public int count() {
		return korder.count();
	}

	@SuppressWarnings("unchecked")
	@Override
	public IPersistentCollection<V> empty() {
		return EPersistentInsertionOrderedSet.EMPTY;
	}

	@Override
	public ISeq<V> seq() {
		return new ESeq<V>(iorder.seq());
	}
	
	static class ESeq<V> implements ISeq<V> {

		ISeq<IMapEntry<Long, V>> iseq;
		
		ESeq(ISeq<IMapEntry<Long, V>> iSeq2) { this.iseq = iSeq2; }
		
		@Override
		public int count() {
			return iseq.count();
		}

		@SuppressWarnings("unchecked")
		@Override
		public IPersistentCollection<V> empty() {
			return EPersistentInsertionOrderedSet.EMPTY;
		}

		@Override
		public boolean equiv(Object arg0) {
			return equals(arg0);
		}

		@Override
		public ISeq<V> seq() {
			return new ESeq<V>(iseq.seq());
		}

		@SuppressWarnings("unchecked")
		@Override
		public ISeq<V> cons(V arg0) {
			return RT.cons(arg0, seq());
		}

		@Override
		public V first() {
			Map.Entry<Long, V> first = iseq.first();
			if (first == null) return null;
			return first.getValue();
		}

		// @SuppressWarnings("unchecked")
		@Override
		public ISeq<V> more() {
			ISeq<IMapEntry<Long, V>> more = iseq.more();
			return new ESeq<V>(more);
		}

		@Override
		public ISeq<V> next() {
			ISeq<IMapEntry<Long, V>> next = iseq.next();
			if (next == null) return null;
			return new ESeq<V>(next);
		}
		
	}
	


}
