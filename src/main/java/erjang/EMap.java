/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2015 by Trifork
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
import java.util.Set;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import com.trifork.clj_ds.IMapEntry;
import com.trifork.clj_ds.IPersistentMap;
import com.trifork.clj_ds.ISeq;
import com.trifork.clj_ds.PersistentHashMap;
import com.trifork.clj_ds.PersistentTreeMap;

import erjang.m.ets.EMatchContext;
import erjang.m.ets.EPattern;
import erjang.m.ets.ETermPattern;

public final class EMap extends EObject {

    public static final EMap EMPTY = new EMap();
	private final IPersistentMap<EObject,EObject> _map;

	
	public EMap() {
        _map = new PersistentTreeMap<EObject, EObject>(null, EObject.ERLANG_ORDERING);
    }

    public EMap(IPersistentMap<EObject,EObject> it) {
        _map = it;
    }

    public EMap testMap() {
        return this;
    }
    
	int cmp_order() { return CMP_ORDER_MAP; }
	
	@Override
	public EAtom is_map() {
		return ERT.TRUE;
	}
	
	@Override
	public EAtom is_map_g() {
        return ERT.TRUE;
	}

	@Override
	int compare_same(EObject rhs) {
		EMap other = rhs.testMap();
		if (other == null) throw ERT.badarg(this, rhs);
		
		int mysize    = _map.count();
		int othersize = other._map.count();
		
		// compare sizes
		if (mysize < othersize) return -1;
		if (othersize > mysize) return 1;
		
		// then compare the keys
		ISeq<IMapEntry<EObject, EObject>> seq1 = _map.seq();
		ISeq<IMapEntry<EObject, EObject>> seq2 = other._map.seq();
				
		while (seq1 != null)
        {
        	IMapEntry<EObject, EObject> ent1 = seq1.first();
        	IMapEntry<EObject, EObject> ent2 = seq2.first();

        	int keycompare = ent1.getKey().erlangCompareTo( ent2.getKey() );
        	if (keycompare != 0) return keycompare;
        	
        	seq1 = seq1.next();
        	seq2 = seq2.next();
        }

		// finally, all keys must be the same so we compare the values
		seq1 = _map.seq();
		seq2 = other._map.seq();
				
		while (seq1 != null)
        {
        	IMapEntry<EObject, EObject> ent1 = seq1.first();
        	IMapEntry<EObject, EObject> ent2 = seq2.first();

        	int keycompare = ent1.getValue().erlangCompareTo( ent2.getValue() );
        	if (keycompare != 0) return keycompare;
        	
        	seq1 = seq1.next();
        	seq2 = seq2.next();
        }

		return 0;
	}

	/**
	 * @return true if this term matches the given matcher
	 */
	public boolean match(ETermPattern matcher, EMatchContext r) {
		return matcher.match(this, r);
	}

	/**
	 * @param out
	 * @return
	 */
	public ETermPattern compileMatch(Set<Integer> out) {
		return EPattern.compilePattern(this, out);
	}
	
	@Override
	public Type emit_const(MethodVisitor mv) {

//		System.out.println("EMIT" +this);
		
		Type type = Type.getType(this.getClass());

		mv.visitFieldInsn(Opcodes.GETSTATIC, type.getInternalName(), "EMPTY", "Lerjang/EMap;");

        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
        		iit != null;
        		iit = iit.next())
        {
        	IMapEntry<EObject, EObject> ent = iit.first();

//        	System.out.println("CONST #{ "+ ent.getKey() + " => " + ent.getValue() + "}");
        	
        	ent.getKey().emit_const(mv);
        	ent.getValue().emit_const(mv);
        	mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, type.getInternalName(), "put", "(Lerjang/EObject;Lerjang/EObject;)Lerjang/EMap;");
        }

		return type;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("#{");
		boolean first = true;
        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
        		iit != null;
        		iit = iit.next())
        {
			if (!first)
				sb.append(',');
			else
				first = false;

        	IMapEntry<EObject, EObject> ent = iit.first();
        	sb.append( ent.getKey() ).append("=>").append(ent.getValue());
        }
		sb.append("}");
		return sb.toString();
	}



    public int hashCode() {
        return to_list().hashCode();
    }

    public EObject get(EObject key) {
        EObject v = _map.valAt(key, null);
        if (v == null) {
            throw ERT.badkey(key);
        }
        return v;
    }

    public EObject get(EObject key, EObject defaultValue) {
        return _map.valAt(key, defaultValue);
    }

    public EObject find(EObject key) {
        EObject v = _map.valAt(key, null);
        if (v == null) {
            return ERT.am_error;
        }
        return new ETuple2(ERT.am_ok, v);
    }

    public EMap put(EObject key, EObject value) {
    	
		EMap res = new EMap( _map.assoc(key, value) );
		
//    	System.out.println( String.valueOf(this) + ".put("+key+","+ value+ ") => "+res);

		return res;
	}

    public EMap update(EObject key, EObject value) {
    	if (!_map.containsKey(key)) throw ERT.badkey(key);
    	return put(key, value);
    }
    
    public EMap remove(EObject key) {
    	if (_map.containsKey(key)) {
			try {
				return new EMap( _map.without(key) );
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
    	} else {
    		return this;
    	}
	}
    
    
    public static EMap from_list(ESeq list) {
        IPersistentMap<EObject,EObject> res = PersistentHashMap.EMPTY;
        while (!list.isNil()) {
            ETuple2 t = ETuple2.cast(list.head());
            if (t == null) {
                throw ERT.badarg(list);
            }
            res = res.assoc(t.elem1, t.elem2);
            list = list.tail();
        }
        return new EMap(res);
    }
    
    // used by codegen
    public boolean has_key(EObject key) {
    	
    	boolean res = _map.containsKey(key);
    	
//    	System.out.println( String.valueOf(this) + ".has_key("+key+") => "+res);
//    	
//    	if (res == false) {
//    		(new Throwable()).printStackTrace();
//    	}
    	
    	return res;
    }
    
    public EAtom is_key(EObject key) {
    	return ERT.box( _map.containsKey(key) );
    }
    
    public ESeq to_list() {
        ESeq it = ERT.NIL;
        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
        		iit != null;
        		iit = iit.next())
        {
        	IMapEntry<EObject, EObject> ent = iit.first();
        	it = it.cons( new ETuple2( ent.getKey(), ent.getValue()  ));
        }
        return it;
    }

    public ESeq keys() {
        ESeq it = ERT.NIL;
        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
        		iit != null;
        		iit = iit.next())
        {
        	IMapEntry<EObject, EObject> ent = iit.first();
        	it = it.cons( ent.getKey() );
        }
        return it;
    }
    
    public ESeq values() {
        ESeq it = ERT.NIL;
        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
        		iit != null;
        		iit = iit.next())
        {
        	IMapEntry<EObject, EObject> ent = iit.first();
        	it = it.cons( ent.getValue() );
        }
        return it;
    }
    
    public EMap merge(EMap other)
    {
    	IPersistentMap<EObject, EObject> res = _map;
        for(ISeq<IMapEntry<EObject, EObject>> iit = other._map.seq();
        		iit != null;
        		iit = iit.next())
        {
        	IMapEntry<EObject, EObject> ent = iit.first();
        	res = res.assoc(ent.getKey(), ent.getValue());
        }
        return new EMap(res);
    }

	public boolean containsKey(EObject key) {
		return _map.containsKey(key);
	}

	public int map_size() {
		return _map.count();
	}

	public EMap put_map_assoc(EObject prefetched2, EObject prefetched3) {
		// TODO Auto-generated method stub
		return null;
	}

	public EMap put_map_exact(EObject prefetched2, EObject prefetched3) {
		// TODO Auto-generated method stub
		return null;
	}

	public static EMap read(EInputStream buf) throws IOException {
		final int arity = buf.read_map_head();

		if (arity > 0) {
		    IPersistentMap<EObject,EObject> _m = EMPTY._map;

		    for (int i = 0; i < arity; i++) {
		    	EObject key = buf.read_any();
		    	EObject value = buf.read_any();
		    	_m = _m.assoc(key, value);
		    }
		    
//		    System.out.println("DID READ "+(new EMap(_m)));
		    
		    return new EMap(_m);
		} else {
			return EMPTY;
		}
	}
    
	@Override
	public void encode(EOutputStream eos) {
		eos.write_map_head(this.map_size());
		
        for(ISeq<IMapEntry<EObject, EObject>> iit = _map.seq();
                iit != null;
                iit = iit.next())
        {
            IMapEntry<EObject, EObject> ent = iit.first();
            eos.write_any(ent.getKey());
            eos.write_any(ent.getValue());
        }
	}
	
}
