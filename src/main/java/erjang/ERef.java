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


package erjang;

import java.io.IOException;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.ETermPattern;

/**
 * 
 */
public final class ERef extends EObject {

	private EAtom node;
	private int creation;
	private int[] ids;

	@Override
	public ERef testReference() {
		return this;
	}
	
	/**
	 * @param node
	 * @param refId
	 * @param creation
	 */
	public ERef(EAtom node, int[] ids, int creation) {
        this.node = node;
        this.creation = creation & 0x03; // 2 bits

        // use at most 82 bits (18 + 32 + 32)
        int len = ids.length;
        this.ids = new int[3];
        this.ids[0] = 0;
        this.ids[1] = 0;
        this.ids[2] = 0;

        if (len > 3) {
            len = 3;
        }
        System.arraycopy(ids, 0, this.ids, 0, len);
        this.ids[0] &= 0x3ffff; // only 18 significant bits in first number
	}

	public ERef(EAtom node, int id1, int id2, int id3, int creation) {
        this.node = node;
        this.creation = creation & 0x03; // 2 bits

        // use at most 82 bits (18 + 32 + 32)
        this.ids = new int[3];
        this.ids[0] = id1 & 0x3ffff; // only 18 significant bits in first number
        this.ids[1] = id2;
        this.ids[2] = id3;
	}

	/**
	 * @param node
	 * @param id
	 * @param creation
	 */
	public ERef(EAtom node, int id, int creation) {
        this.node = node;
        ids = new int[1];
        ids[0] = id & 0x3ffff; // 18 bits
        this.creation = creation & 0x03; // 2 bits
	}

	@Override
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}
	
	@Override
	int compare_same(EObject rhs) {

		int val = compare_same2(rhs);
		// System.err.println("compare_same "+this+" <=> "+rhs+"  ==> "+val);
    	return val;
	}
    
	int compare_same2(EObject rhs) {

        	// System.err.println("compare_same "+this+" <=> "+rhs);

        	ERef oref = (ERef) rhs;
		
		int cmp = node.compareTo(oref.node);
		if (cmp != 0) return cmp;
		
		cmp = cmp(creation, oref.creation);
		if (cmp != 0) return cmp;
		
		cmp = cmp(ids.length, oref.ids.length);
		if (cmp != 0) return cmp;
		
		for (int i = 0; i < ids.length; i++) {
			cmp = cmp(ids[i], oref.ids[i]);
			if (cmp != 0) return cmp;
		}
		
		return 0;
	}

	private static int cmp(int i1, int i2) {
		if (i1==i2) return 0;
		if (i1<i2) return -1;
		return 1;
	}
	
    /**
     * Determine if two refs are equal. Refs are equal if their components are
     * equal. New refs and old refs are considered equal if the node, creation
     * and first id numnber are equal.
     * 
     * @param o
     *                the other ref to compare to.
     * 
     * @return true if the refs are equal, false otherwise.
     */
    @Override
	public boolean equals(Object rhs) {

    	boolean val = equals2(rhs);
    	// System.err.println("equals "+this+" =:= "+rhs+"  ==> "+val);
    	return val;
	}

    public boolean equals2(final Object o) {
    	
        if (!(o instanceof ERef)) {
            return false;
        }

        final ERef ref = (ERef) o;

        if ((node != ref.node()) || creation != ref.creation()) {
            return false;
        }

		// TODO: Compare the isNewRef()s ?
        if (isNewRef() && ref.isNewRef()) {
            return ids[0] == ref.ids[0]
                && ids[1] == ref.ids[1]
                && ids[2] == ref.ids[2];
        }
        
        return ids[0] == ref.ids[0];
    }

	@Override
	public int hashCode() {
		int res =
			500000003  * node.hashCode() +
			1000000007 * creation +
			1500000001 * ids[0];
		if (isNewRef()) {
			res +=
				ids[1] * 250000013 +
				ids[2] * 750000007;
		}
		return res;
	}

	@Override
	int cmp_order() {
		return CMP_ORDER_REFERENCE;
	}

    /**
     * Get the id number from the ref. Old style refs have only one id number.
     * If this is a new style ref, the first id number is returned.
     * 
     * @return the id number from the ref.
     */
    public int id() {
        return ids[0];
    }

    /**
     * Get the array of id numbers from the ref. If this is an old style ref,
     * the array is of length 1. If this is a new style ref, the array has
     * length 3.
     * 
     * @return the array of id numbers from the ref.
     */
    public int[] ids() {
        return ids;
    }

    /**
     * Determine whether this is a new style ref.
     * 
     * @return true if this ref is a new style ref, false otherwise.
     */
    public boolean isNewRef() {
        return ids.length > 1;
    }

    /**
     * Get the creation number from the ref.
     * 
     * @return the creation number from the ref.
     */
    public int creation() {
        return creation;
    }

    /**
     * Get the node name from the ref.
     * 
     * @return the node name from the ref.
     */
    public EAtom node() {
        return node;
    }

    /**
     * Get the string representation of the ref. Erlang refs are printed as
     * #Ref&lt;node.id&gt;
     * 
     * @return the string representation of the ref.
     */
    @Override
    public String toString() {
        String s = "#Ref<" + node + "." + creation();

        for (int i = ids.length-1; i >= 0; i--) {
            s += "." + ids[i];
        }

        s += ">";

        return s;
    }

	/**
	 * @return
	 */
	public int[] internal_ref_numbers() {
		throw new NotImplemented();
	}

	public static ERef read(EInputStream ei) throws IOException {
		return ei.read_ref();
	}

	/* (non-Javadoc)
	 * @see erjang.EObject#encode(erjang.EOutputStream)
	 */
	@Override
	public void encode(EOutputStream eos) {
		eos.write_ref(node.getName(), ids, creation);
	}
}
