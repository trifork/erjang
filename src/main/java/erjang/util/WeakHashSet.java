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

package erjang.util;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A container of objects that do disallow garbage collection
 */
public class WeakHashSet<T> extends AbstractSet<T> {

	Map<WO, WO> elementSet = new ConcurrentHashMap<WO, WO>();

	ReferenceQueue<T> referenceQueue = new ReferenceQueue<T>();

	private static Object NULL = new Object();

	protected int hashCode(T val) {
		return val.hashCode();
	}
	
	protected boolean equals(T v1, T v2) {
		return v1.equals(v2);
	}
	
	class WO extends WeakReference<T> {

		private int hashCode;

		/**
		 * @param referent
		 * @param q
		 * @param q
		 */
		public WO(T referent, int hash, ReferenceQueue<? super T> q) {
			super(referent, q);
			this.hashCode = hash;
		}

		@Override
		public int hashCode() {
			return hashCode;
		}

		@Override
		public boolean equals(Object obj) {

			// this test is needed for "remove" after an object has been
			// garbage collected.
			if (this == obj)
				return true;

			WO other = (WO) obj;

			if (hashCode != other.hashCode) {
				return false;
			}

			T v1 = this.get();
			T v2 = other.get();

			if (v1 == null || v2 == null)
				return false;

			return v1.equals(v2);
		}
	}

	private void cleanup() {
		WO weak;
		while ((weak = (WO) referenceQueue.poll()) != null) {
			elementSet.remove(weak);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean contains(Object e) {
		cleanup();
		WO o = new WO((T) (e == null ? NULL : e), hashCode((T) e), referenceQueue);
		return elementSet.containsKey(o);
	};

	@SuppressWarnings("unchecked")
	public boolean add(T e) {
		cleanup();
		WO o = new WO((T) (e == null ? NULL : e), hashCode(e), referenceQueue);
		if (elementSet.containsKey(o))
			return false;
		elementSet.put(o, o);
		return true;
	};

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.AbstractCollection#remove(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public boolean remove(Object e) {
		try {
			WO o = new WO((T) (e == null ? NULL : e), hashCode((T) e), referenceQueue);
			return elementSet.remove(o) != null;
		} finally {
			cleanup();
		}
	}

	@Override
	public Iterator<T> iterator() {
		return new Iterator<T>() {

			final Iterator<WO> iter = elementSet.keySet().iterator();
			private T next;

			@Override
			public boolean hasNext() {
				while (iter.hasNext()) {

					WO weak = iter.next();
					T obj = null;
					if (weak != null && (obj = weak.get()) == null) {
						// object has been reclaimed by the GC
						continue;
					}

					next = obj;
					return true;
				}
				return false;
			}

			@Override
			public T next() {
				if ((next == null) && !hasNext()) {
					throw new NoSuchElementException();
				}

				T obj = next;
				next = null;

				return (obj==NULL ? null : obj);
			}

			@Override
			public void remove() {
				iter.remove();
			}
		};
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.AbstractCollection#size()
	 */
	@Override
	public int size() {
		cleanup();
		return elementSet.size();
	}

}
