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

import kilim.Pausable;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 
 */
public class ELocalNode extends EAbstractNode {

    private int serial = 0;
    private int pidCount = 1;
    private int portCount = 1;
    private int refId[];

    
    protected ELocalNode() {
        super();
        init();
    }

    /**
     * Create a node with the given name and the default cookie.
     */
    protected ELocalNode(final EAtom node) {
        super(node);
        init();
    }

    /**
     * Create a node with the given name and cookie.
     */
    protected ELocalNode(final EAtom node, final EAtom cookie) {
        super(node, cookie);
        init();
    }


    
    
    private void init() {
        serial = 0;
        pidCount = 1;
        portCount = 1;
        refId = new int[3];
        refId[0] = 1;
        refId[1] = 0;
        refId[2] = 0;
    }

    /** Reference serial counters are valid when low bit of
     * ref_serial_high is equal to high bit of ref_serial_low. */
    final static AtomicLong ref_serial_low = new AtomicLong();
    final static AtomicInteger ref_serial_high = new AtomicInteger();
    public ERef createRef() {
	/* This algorithm technically has the flaw that it may break
	 * if > 2^63 time steps pass between these two reads...
	 * but I think we can consider that unlikely.
	 * -- Erik Søe Sørensen */
	long low = ref_serial_low.incrementAndGet();
	int high = ref_serial_high.get();

	// Check validity:
	if ((low >>> 63) != (high & 1)) {
	    // Adjust high:
	    ref_serial_high.compareAndSet(high, ++high);
	    // Don't mind if we fail; that just means someone else succeeded.
	}

	// Pick 18 + 32 + 32 bits:
	int id1 = (int) (low & ((1 << 18) - 1));
	int id2 = (int)(low >> 18);
	int id3 = (int)((low >> (18+32)) | (high << (63-(18+32)))); // 1 bit overlap

	return new ERef(node, id1, id2, id3, creation);
    }

    public synchronized int createPortID() {
        portCount++;
        if (portCount > 0xfffffff) { /* 28 bits */
            portCount = 0;
        }
        return portCount;
    }
    
	public EObject dsig_reg_send(EInternalPID caller, EAtom name,
			EObject msg) throws Pausable {
		return ERT.send(caller.task(), name, msg);
	}

	public void dsig_demonitor(EHandle sender, ERef ref,
			EObject to_pid_or_name) throws Pausable
	{
		EInternalPID pid;
		EAtom name;
		if ((pid=to_pid_or_name.testInternalPID()) != null) {
			EProc task = pid.task();
			if (task != null) task.demonitor(ref);
		} else if ((name=to_pid_or_name.testAtom()) != null) {
			EObject local = ERT.whereis(name);
			if ((pid=local.testInternalPID()) != null) {
				EProc task = pid.task();
				if (task != null) task.demonitor(ref);
			}
		}
	}

    
}
