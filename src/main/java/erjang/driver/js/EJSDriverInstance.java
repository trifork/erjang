/**
 */

/* THIS FILE IS DERIVED FROM spidermonkey_drv.c 
 * ORIGINAL COPYRIGHT NOTICE 

 author Kevin Smith <ksmith@basho.com>
 copyright 2009-2010 Basho Technologies

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. */

package erjang.driver.js;

import java.io.IOException;
import java.nio.ByteBuffer;

import kilim.Pausable;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.Script;
import org.mozilla.javascript.ScriptableObject;

import erjang.EAtom;
import erjang.EBinary;
import erjang.EHandle;
import erjang.ERT;
import erjang.ETuple;
import erjang.driver.EAsync;
import erjang.driver.EDriver;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;

public class EJSDriverInstance extends EDriverInstance {

	static final EAtom am_ok = ERT.am_ok;
	static final EAtom am_error = ERT.am_error;
	static final EAtom am_unknown_command = EAtom.intern("unknown_command");

	static ContextFactory cf_global = ContextFactory.getGlobal();
	
	static class EContext extends Context {
		public EContext() {
			super(cf_global);
		}
	}
	
	public EJSDriverInstance(EDriver driver) {
		super(driver);
	}

	void send_ok_response(EBinary call_id) throws Pausable {
		driver_output_term(ETuple.make(call_id, am_ok));
	}

	void send_error_string_response(EBinary call_id, String msg)
			throws Pausable {
		driver_output_term(ETuple.make(call_id, am_error, EBinary
				.fromString(msg)));
	}

	void send_string_response(EBinary call_id, String result) throws Pausable {
		driver_output_term(ETuple.make(call_id, am_ok, EBinary
				.fromString(result)));
	}

	void unknown_command(EBinary call_id) throws Pausable {
		driver_output_term(ETuple.make(call_id, am_error, am_unknown_command));
	}

	short IJ = ('i' << 8) | 'j'; 
	short EJ = ('e' << 8) | 'j'; 
	short DJ = ('d' << 8) | 'j'; 
	short SD = ('s' << 8) | 'd';
	
	private VM vm; 
	
	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		data.ready();
	}


	@Override
	protected void output(EHandle caller, final ByteBuffer buf) throws IOException,
			Pausable {

		final int cmd = buf.getShort();
		final EBinary call_id = read_binary(buf);
		
		if (cmd == IJ) {
			int heap_size = buf.getInt();
			vm = sm_initialize(heap_size * 1024 * 1024);
			send_ok_response(call_id);
			
		} else {
			
			EAsync job = new EAsync() {
				
				String err, resp;
				
				// called in separate thread
				@Override
				public void async() {

					if (cmd == EJ) {
	
						String filename = read_string(buf);
						String code = read_string(buf);

						String result = sm_eval(filename, code, true);
						if (result.startsWith("{\"error\"")) {
							err = result;
						} else {
							resp = result;
						}
						
					} else if (cmd == DJ) {
						
						String filename = read_string(buf);
						String code = read_string(buf);

						String result = sm_eval(filename, code, false);
						if (result != null) {
							err = result;
						}
						
						
					} else if (cmd == SD) {
						vm = null;
					} 
					
				}
				
				// called when done
				@Override
				public void ready() throws Pausable {

					if (err != null) {
						send_error_string_response(call_id, err);
					} else if (resp != null) {
						send_string_response(call_id, err);
					} else {
						send_ok_response(call_id);
					}
					
				}
				
			};
			
			driver_async(job);
			
		}
	}

	static class VM {
		EContext cx;
		ScriptableObject global;
	}
	
	private VM sm_initialize(int i) {
		
		VM vm = new VM();
		
		vm.cx = new EContext();
		cf_global.enterContext(vm.cx);

		try {
			vm.global = vm.cx.initStandardObjects();

			return vm;
		} finally {	
			Context.exit();
		}		
	}

	private String sm_eval(String filename, String code, boolean handle_retval) {
		
		Context ctx = cf_global.enterContext(vm.cx);
		try {
			
			Script scr = ctx.compileString(code, filename, 1, null);			
			Object result = scr.exec(ctx, vm.global);
			
			if (handle_retval) {
				if (result instanceof String) {
					return (String) result;
				} else if (result == Context.getUndefinedValue()){
					return "{\"error\": \"Expression returned undefined\", \"lineno\": 0, \"source\": \"unknown\"}";
				} else {
					return "{\"error\": \"non-JSON return value\", \"lineno\": 0, \"source\": \"unknown\"}";
				}
			} else {
				return null;
			}
			
		} finally {
			Context.exit();
		}
		
	}


	private EBinary read_binary(ByteBuffer buf) {
		int str_len = buf.getInt();
		EBinary str = new EBinary(buf.array(),
				buf.position()+buf.arrayOffset(), str_len);
		buf.position(buf.position()+str_len);
		return str;
	}

	private String read_string(ByteBuffer buf) {
		int str_len = buf.getInt();
		byte[] data = new byte[str_len];
		buf.get(data);
		return new String(data, IO.UTF8);
	}


}
