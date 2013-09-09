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

import java.util.Iterator;
import java.util.Map;

import com.trifork.clj_ds.APersistentMap;
import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentCollection;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.PersistentTreeMap;
import com.trifork.clj_ds.RT;

@SuppressWarnings({ "serial", "unchecked" })
public class EPersistentInsertionOrderedMap<K,V> extends APersistentMap<K,V> {

	
	public static class EIterator<K, V> implements Iterator<Entry<K, V>> {

		private Iterator<? extends java.util.Map.Entry<? extends Object, ? extends Map.Entry<K, V>>> it;

		public EIterator(
				Iterator<? extends java.util.Map.Entry<? extends Object, ? extends Map.Entry<K, V>>> it) {
					this.it = it;
		}

		@Override
		public boolean hasNext() {
			return it.hasNext();
		}

		@Override
		public java.util.Map.Entry<K, V> next() {
			return it.next().getValue();
		}

		@Override
		public void remove() {
			it.remove();
		}

	}

	private final IPersistentMap<Long, Rec<K,V>> iorder;
	private final IPersistentMap<K, Rec<K,V>> korder;
	private final long ins;
	
	@SuppressWarnings({ "rawtypes" })
	public static EPersistentInsertionOrderedMap EMPTY = new EPersistentInsertionOrderedMap(0, PersistentTreeMap.EMPTY, PersistentHashMap.EMPTY);

	private EPersistentInsertionOrderedMap(long ins, IPersistentMap<Long,Rec<K,V>> iorder, IPersistentMap<K,Rec<K,V>> keyorder)
	{
		this.ins = ins+1;
		this.iorder = iorder;
		this.korder = keyorder;		
	}
	
	static class Rec<K,V> implements IMapEntry<K, V>, Map.Entry<K, V> {
		
		private K key;
		private V value;
		private Long ins;

		Rec(Long ins, K key, V value) {
			this.ins = ins;
			this.key = key;
			this.value = value;
		}

		@Override
		public K getKey() {
			return key;
		}

		@Override
		public V getValue() {
			return value;
		}

		@Override
		public V setValue(V value) {
			throw new UnsupportedOperationException();
		}

		@Override
		public K key() {
			return key;
		}

		@Override
		public V val() {
			return value;
		}
		
		@Override
		public boolean equals(Object obj) {
			if (obj == this) return true;
			IMapEntry<K, V> o = (IMapEntry<K, V>) obj;
			
			return o.key().equals(key) && o.val().equals(value);
		}
	}
	
	Rec<K,V> rec(K key, V value) {
		return new Rec<K,V>(ins, key, value);
	}
	
	@Override
	public IPersistentMap<K, V> assoc(K key, V value) {
		Rec<K,V> nrec = rec(key, value);
		Rec<K,V> orec = korder.valAt(key);
		IPersistentMap<Long, Rec<K,V>> i = iorder;
		if (orec != null) {
			try {
				i = i.without(orec.ins);
			} catch (Exception e) {
				// ignore //
			}
		} 
		i = i.assoc(nrec.ins, nrec);
		IPersistentMap<K, Rec<K, V>> k = korder.assoc(key, nrec);
		return make(i, k);
	}


	private IPersistentMap<K, V> make(IPersistentMap<Long, Rec<K, V>> i,
			IPersistentMap<K, Rec<K, V>> k) {
		return new EPersistentInsertionOrderedMap<K,V>(ins, i, k);
	}

	@Override
	public IPersistentMap<K, V> assocEx(K key, V value) throws Exception {
		Rec<K,V> nrec = rec(key, value);
		Rec<K,V> orec = korder.valAt(key);
		IPersistentMap<Long, Rec<K,V>> i = iorder;
		if (orec != null) {
			try {
				i = i.without(orec.ins);
			} catch (Exception e) {
				// ignore //
			}
		} 
		IPersistentMap<K, Rec<K, V>> k = korder.assocEx(key, nrec);
		i = i.assocEx(nrec.ins, nrec);
		return make(i, k);
	}

	@Override
	public Iterator<java.util.Map.Entry<K, V>> iteratorFrom(K key) {		
		return new EIterator<K,V>( korder.iteratorFrom(key) );
	}

	@Override
	public Iterator<java.util.Map.Entry<K, V>> reverseIterator() {
		return new EIterator<K,V>( iorder.reverseIterator() );
	}

	@Override
	public Iterator<java.util.Map.Entry<K, V>> iterator() {
		return new EIterator<K,V>( iorder.iterator() );
	}


	@Override
	public IPersistentMap<K, V> without(K key) throws Exception {
		Rec<K,V> orec = korder.valAt(key);
		IPersistentMap<K, Rec<K, V>> k = korder;
		IPersistentMap<Long, Rec<K,V>> i = iorder;
		if (orec != null) {
			try {
				k = k.without(key);
				i = i.without(orec.ins);
			} catch (Exception e) {
				// ignore //
			}
		} 
		return make(i, k);
	}

	@Override
	public boolean containsKey(Object arg0) {
		return korder.containsKey((K) arg0);
	}

	@Override
	public IMapEntry<K, V> entryAt(final K key) {
		Rec<K,V> rec = korder.valAt(key);
		if (rec == null) return null;
		return rec;
	}

	@Override
	public int count() {
		return korder.count();
	}

	@Override
	public IPersistentCollection<IMapEntry<K, V>> empty() {
		return EPersistentInsertionOrderedMap.EMPTY;
	}

	@Override
	public ISeq<IMapEntry<K, V>> seq() {
		return new ESeq<K,V>(iorder.seq());
	}
	
	static class ESeq<K,V> implements ISeq<IMapEntry<K, V>> {

		ISeq<IMapEntry<Long, Rec<K,V>>> iseq;
		
		ESeq(ISeq<IMapEntry<Long, Rec<K,V>>> iseq) { this.iseq = iseq; }
		
		@Override
		public int count() {
			return iseq.count();
		}

		@Override
		public IPersistentCollection<IMapEntry<K, V>> empty() {
			return EPersistentInsertionOrderedMap.EMPTY;
		}

		@Override
		public boolean equiv(Object arg0) {
			return equals(arg0);
		}

		@Override
		public ISeq<IMapEntry<K, V>> seq() {
			return new ESeq<K,V>(iseq.seq());
		}

		@Override
		public ISeq<IMapEntry<K, V>> cons(IMapEntry<K, V> arg0) {
			return RT.cons(arg0, seq());
		}

		@Override
		public IMapEntry<K, V> first() {
			IMapEntry<Long, Rec<K, V>> first = iseq.first();
			if (first == null) return null;
			return first.getValue();
		}

		// @SuppressWarnings("unchecked")
		@Override
		public ISeq<IMapEntry<K, V>> more() {
			ISeq<IMapEntry<Long, Rec<K, V>>> more = iseq.more();
			return new ESeq<K,V>(more);
		}

		@Override
		public ISeq<IMapEntry<K, V>> next() {
			ISeq<IMapEntry<Long, Rec<K, V>>> next = iseq.next();
			if (next == null) return null;
			return new ESeq<K,V>(next);
		}
		
	}
	

	@Override
	public V valAt(K arg0) {
		Rec<K, V> rec = korder.valAt(arg0);
		if (rec == null) return null;
		return rec.value;
	}

	@Override
	public V valAt(K arg0, V arg1) {
		V v = valAt(arg0);
		if (v == null) v = arg1;
		return v;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void putAll(Map m) {
		throw new UnsupportedOperationException();		
	}



}
