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


    public synchronized ERef createRef() {
        final ERef r = new ERef(node, refId, creation);

        // increment ref ids (3 ints: 18 + 32 + 32 bits)
        refId[0]++;
        if (refId[0] > 0x3ffff) {
            refId[0] = 0;

            refId[1]++;
            if (refId[1] == 0) {
                refId[2]++;
            }
        }

        return r;

	}
	
}
