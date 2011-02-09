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

package erjang.driver;

import static erjang.EPort.am_arg0;
import static erjang.EPort.am_args;
import static erjang.EPort.am_binary;
import static erjang.EPort.am_cd;
import static erjang.EPort.am_close;
import static erjang.EPort.am_env;
import static erjang.EPort.am_eof;
import static erjang.EPort.am_exit_status;
import static erjang.EPort.am_hide;
import static erjang.EPort.am_in;
import static erjang.EPort.am_line;
import static erjang.EPort.am_nouse_stdio;
import static erjang.EPort.am_out;
import static erjang.EPort.am_packet;
import static erjang.EPort.am_stream;
import static erjang.EPort.am_use_stdio;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import kilim.Pausable;
import kilim.Task;
import erjang.EAtom;
import erjang.EBinary;
import erjang.ECons;
import erjang.EHandle;
import erjang.EInternalPID;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPeer;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ERef;
import erjang.ESeq;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;
import erjang.ETuple4;
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.ErlangExit;
import erjang.ErlangExitSignal;
import erjang.NotImplemented;

/**
 * Base class for the two kinds of driver tasks: drivers, and "exec"s
 */
public abstract class EDriverTask extends ETask<EInternalPort> implements
		NIOHandler {

	static Logger log = Logger.getLogger(EProc.class.getName());

	public abstract EObject getName();
	
	@Override
	public String toString() {
		return "<driver_task:" + super.id + ">";
	}

	private static final EAtom am_name = EAtom.intern("name");
	private static final EAtom am_data = EAtom.intern("data");
	private static final EAtom am_connected = EAtom.intern("connected");
	private static final EAtom am_closed = EAtom.intern("closed");
	private final EInternalPort port;
	protected EPID owner;
	private final EDriverControl instance;

	
	public EPID owner() {
		return owner;
	}
	
	public void owner(EInternalPID ipid) {
		this.owner = ipid;
	}
	
	private	static ConcurrentHashMap<Integer,EDriverTask> all_ports 
		= new ConcurrentHashMap<Integer,EDriverTask> ();

	public EDriverTask(EPID owner, EDriverControl driver) {

		EDriver drv = driver.getDriver();
		if (drv.useDriverLevelLocking() == true) {
			System.err.println("DRIVER_LEVEL_LOCK: "+driver);
			driver = new LockingDriverInstance(driver, drv.getLock());
		} else {
			driver = new LockingDriverInstance(driver, new kilim.ReentrantLock());
		}
		
		this.owner = owner;
		this.instance = driver;
		this.port = new EInternalPort(this);
		driver.setTask(this);
		

		all_ports.put(id, this);
	}

	public void setupInstance() {instance.setup();}

	@Override
	public EInternalPort self_handle() {
		return port;
	}

	static enum Mode {
		STREAM, PACKET, LINE
	};

	protected boolean send_binary_data;
	protected boolean is_out_only;
	protected boolean is_in_only;
	protected boolean send_eof;
	protected boolean hide;
	protected int port_out_fd;
	protected int port_in_fd;
	protected boolean send_exit_status;
	protected int packet;
	protected int line_length;
	protected Mode mode = Mode.STREAM;
	protected String[] cmd;
	protected String cwd;
	protected HashMap<String, String> env;
	private long abs_timeout;

	/** state controlled from elsewhere... erlang:port_set_data/2*/
	public EObject port_data;
	
	public static final int ERTS_PORT_SFLG_CONNECTED = 1<<0;
	public static final int ERTS_PORT_SFLG_EXITING = 1<<1;
	public static final int ERTS_PORT_SFLG_DISTRIBUTION = 1<<2;
	public static final int ERTS_PORT_SFLG_BINARY_IO = 1<<3;
	public static final int ERTS_PORT_SFLG_SOFT_EOF = 1<<4;
	public static final int ERTS_PORT_SFLG_PORT_BUSY = 1<<5;
	public static final int ERTS_PORT_SFLG_CLOSING = 1<<6;
	public static final int ERTS_PORT_SFLG_SEND_CLOSED = 1<<7;
	public static final int ERTS_PORT_SFLG_LINEBUF_IO = 1<<8;
	public static final int ERTS_PORT_SFLG_IMMORTAL = 1<<9;
	public static final int ERTS_PORT_SFLG_FREE = 1<<10;
	public static final int ERTS_PORT_SFLG_FREE_SCHEDULED = 1<<11;
	public static final int ERTS_PORT_SFLG_INITIALIZING = 1<<12;
	public static final int ERTS_PORT_SFLG_PORT_SPECIFIC_LOCK = 1<<13;
	public static final int ERTS_PORT_SFLG_INVALID = 1<<14;
	public static final int ERTS_PORT_SFLG_DEBUG = 1<<31;
	
	public int status;
	private EPeer peer;
	boolean stderr_to_stdout;
	private EPID reply_closed_to;

	/**
	 * @param cmd
	 * @param portSetting
	 */
	protected void parseOptions(String[] cmd, EObject portSetting) {
		// TODO: most of this is way too expensive for non-exec ports

		// set by options
		this.cmd = cmd;
		this.cwd = System.getProperty("user.dir");
		this.env = new HashMap<String, String>(System.getenv());

		this.packet = -1; // not set
		this.line_length = -1;

		this.send_exit_status = false;
		this.stderr_to_stdout = false;
		this.port_in_fd = 1;
		this.port_out_fd = 2;

		this.hide = false;
		this.send_eof = false;

		this.is_in_only = false;
		this.is_out_only = false;
		this.send_binary_data = false;

		ECons settings = portSetting.testCons();
		if (settings == null)
			throw ERT.badarg();

		for (; settings != null && !settings.isNil(); settings = settings
				.tail().testCons()) {

			EObject val = settings.head();
			ETuple tup;
			if ((tup = val.testTuple()) != null) {
				ETuple2 tup2;
				if ((tup2 = ETuple2.cast(tup)) != null) {

					if (tup2.elem1 == am_args) {
						ESeq list = tup2.elem2.testSeq();
						EObject[] nargs = list.toArray();

						String[] new_cmd = new String[nargs.length + 1];
						new_cmd[0] = cmd[0];
						for (int i = 0; i < nargs.length; i++) {
							new_cmd[i + 1] = EString.make(nargs[i])
									.stringValue();
						}
						cmd = new_cmd;

					} else if (tup2.elem1 == am_arg0) {
						String[] new_cmd = new String[2];
						new_cmd[0] = cmd[0];
						new_cmd[1] = EString.make(tup2.elem2).stringValue();

					} else if (tup2.elem1 == am_packet) {
						packet = tup2.elem2.asInt();
						mode = Mode.PACKET;

					} else if (tup2.elem1 == am_cd) {
						cwd = EString.make(tup2.elem2).stringValue();

					} else if (tup2.elem1 == am_env) {

						ESeq ee;
						if ((ee = tup2.elem2.testSeq()) == null) {
							throw ERT.badarg();
						}

						EObject[] envs = ee.toArray();
						for (int i = 0; i < envs.length; i++) {
							ETuple2 e = ETuple2.cast(envs[i].testTuple());
							if (e.elem2 == ERT.FALSE) {
								env.remove(EString.make(e.elem1).stringValue());
							} else {
								env.put(EString.make(e.elem1).stringValue(),
										EString.make(e.elem2).stringValue());
							}
						}

					} else if (tup2.elem1 == am_line) {
						line_length = tup2.elem2.asInt();
						mode = Mode.LINE;

					} else {

						throw ERT.badarg();
					}

				}
			} else if (val == am_stream) {
				mode = Mode.STREAM;

			} else if (val == am_use_stdio) {
				port_in_fd = 1;
				port_out_fd = 2;

			} else if (val == am_nouse_stdio) {
				port_in_fd = 3;
				port_out_fd = 4;

			} else if (val == am_hide) {
				hide = true;

			} else if (val == am_exit_status) {
				send_exit_status = true;

			} else if (val == EPort.am_stderr_to_stdout) {
				stderr_to_stdout = true;

			} else if (val == am_eof) {
				send_eof = true;

			} else if (val == am_in) {
				is_in_only = true;

			} else if (val == am_out) {
				is_out_only = true;

			} else if (val == am_binary) {
				send_binary_data = true;

			}
		}

	}

	@Override
	public Task start() {
		Task result = super.start();
		this.pstate = STATE_RUNNING;
		return result;
	}
	
	@Override
	public void execute() throws Pausable {
		try {

			EObject result = null;
			try {

				// driver main loop
				main_loop();

				result = am_normal;

			} catch (NotImplemented e) {
				log.log(Level.SEVERE, "exiting "+self_handle(), e);
				result = e.reason();

			} catch (ErlangException e) {
				// e.printStackTrace();
				result = e.reason();

			} catch (ErlangExitSignal e) {
				// e.printStackTrace();
				result = e.reason();

			} catch (Throwable e) {

				e.printStackTrace();

				ESeq erl_trace = ErlangError.decodeTrace(e.getStackTrace());
				ETuple java_ex = ETuple.make(am_java_exception, EString
						.fromString(ERT.describe_exception(e)));

				result = ETuple.make(java_ex, erl_trace);
				
				if (ERT.DEVEL) {
					System.err.println("EXITING "+result);
					System.exit(1);
				}

			} finally {
				// this.runner = null;
				this.pstate = STATE_DONE;
			}

			// System.err.println("task "+this+" exited with "+result);
			do_proc_termination(result);

		} catch (ThreadDeath e) {
			throw e;

		} catch (Throwable e) {
			e.printStackTrace();
		}

	}

	/**
	 * @throws Pausable
	 * 
	 */
	protected void main_loop() throws Exception, Pausable {

		/** out is used locally later, but we allocate it once and for all. */
		List<ByteBuffer> out = new ArrayList<ByteBuffer>();
		EObject msg;

		next_message: while (true) {

			/** if the driver has a registered timeout ... handle that */
			if (abs_timeout == 0) {
				msg = mbox.get();
			} else {
				msg = null;
				long timeout = abs_timeout - System.currentTimeMillis();
				if (timeout > 0) {
					msg = mbox.get(timeout);
				} 
				if (msg == null) {
					abs_timeout = 0;
					this.instance.timeout();
					continue next_message;
				}
			}

			ETuple2 t2;
			EPortControl ctrl;
			ETuple3 t3;
			ETuple4 t4;
			ETuple tup;
			if ((t2 = ETuple2.cast(msg)) != null) {

				EObject sender = t2.elem1;

				ETuple2 cmd;
				if ((cmd = ETuple2.cast(t2.elem2)) != null) {
					// cmd must be one of
					// {command, iodata()}
					// {connect, PID}

					if (cmd.elem1 == EPort.am_command) {
						if (cmd.elem2.collectIOList(out)) {
							EHandle caller = sender.testHandle();
							
							if (caller == null) {
								System.err.println("*** sender is null? "+sender);
							}
							
							if (out.size() == 0) {
								instance.outputv(caller, ERT.EMPTY_BYTEBUFFER_ARR);
							} else {
								instance.outputv(caller, out.toArray(new ByteBuffer[out
										.size()]));
							}
							// if collectIOList fails, do the port task die?
							// and how?
						}
						
						out.clear();

						continue next_message;

					} else if (cmd.elem1 == EPort.am_connect) {
						EPID new_owner;
						if ((new_owner = cmd.elem2.testPID()) == null)
							break;

						EPID old_owner = this.owner;
						this.owner = new_owner;

						old_owner.send(this.port, ETuple.make(this.self_handle(),
										EPort.am_connected));

						continue next_message;

					}

				} else if (t2.elem2 == am_close) {
					this.reply_closed_to = t2.elem1.testPID();
					// will call instance.stop()
					return;
				}

			} else if ((ctrl = msg.testPortControl()) != null) {

				// port control messages are simply run
				ctrl.execute();
				continue next_message;

			} else if ((t3 = ETuple3.cast(msg)) != null) {

				// {'EXIT', From, Reason} comes in this way
				if (t3.elem1 == ERT.am_EXIT) {
					// close is handled by exception handling code
					return;
				}
			} else if ((tup = msg.testTuple()) != null && tup.arity() == 5) {
				// {'DOWN', ref, process, pid, reason}
				if (tup.elm(1) == ERT.am_DOWN) {
					ERef ref = tup.elm(2).testReference();
					instance.processExit(ref);
				}

			}

			break;
		}

		throw new ErlangError(ERT.am_badsig, msg);
	}

	/**
	 * implementation of port_control
	 * @param caller 
	 * 
	 * @param op
	 * @param cmd2
	 * @throws Pausable 
	 */
	public EObject control(EProc caller, int op, ByteBuffer cmd2) throws Pausable {

		if (pstate == STATE_RUNNING || pstate == STATE_INIT) {
			// ok
		} else {
			System.err.println("port "+this.self_handle()+" in state: "+pstate);
			throw ERT.badarg();
		}

		if (ERT.DEBUG_PORT) 
			System.out.println("ctrl: cmd="+op+"; arg="+EBinary.make(cmd2));
		
		while (mbox.hasMessage()) {
			Task.yield();
		}		
		
		long old_to = abs_timeout;
		ByteBuffer bb = instance.control(caller.self_handle(), op, cmd2);
		long new_to = abs_timeout;
		
		if (old_to != new_to) {
			mbox.put(new EPortControl() {
				@Override
				public void execute() throws Pausable, IOException {
					// do nothing, just trigger main loop
				}
			});
		}
		
		if (bb == null || bb.position() == 0) {
				return ERT.NIL;

		} else {

			bb.flip();
			return EString.make(bb.array(), bb.arrayOffset(), bb.remaining());
		}
	}

	/**
	 * @param out
	 * @return
	 */
	public static ByteBuffer flatten(ByteBuffer[] out) {
		if (out.length == 0) {
			return ERT.EMPTY_BYTEBUFFER;
		} else if (out.length == 1) {
			return out[0];
		} else {
			long size = 0;
			for (int i = 0; i < out.length; i++) {
				size += out[i].limit();
			}
			if (size > Integer.MAX_VALUE)
				throw new IllegalArgumentException("buffer too large to flatten "+size);
			
			ByteBuffer res = ByteBuffer.allocate((int)size);
			for (int i = 0; i < out.length; i++) {
				res.put(out[i]);
			}
			res.flip();
			return res;
		}
	}

	/**
	 * @param op
	 * @param data
	 * @return
	 * @throws Pausable 
	 */
	public EObject call(EProc caller, int op, EObject data) throws Pausable {
		if (pstate != STATE_RUNNING) {
			throw ERT.badarg();
		}

		EObject result = instance.call(caller.self_handle(), op, data);

		if (result == null) {
			return ERT.NIL;
		} else {
			return result;
		}
	}

	/**
	 * erlang:port_command uses this, since error handling happens in the BIF
	 * @param caller TODO
	 * @param out
	 * 
	 * @return
	 * @throws Pausable
	 */
	public void command(final EHandle caller, final ByteBuffer[] out) throws Pausable {
		mbox.put(new EPortControl() {
			@Override
			public void execute() throws Pausable, IOException {
				instance.outputv(caller, out);
			}
		});
	}
	
	public void close() throws Pausable {
		mbox.put(new EPortControl() {
			
			@Override
			public void execute() throws Exception, Pausable {
				throw new ErlangExitSignal(am_normal);
			}
			
		});
	}

	/** our owner died, do something! */
	@Override
	protected void process_incoming_exit(EHandle from, EObject reason, boolean exitToSender) throws Pausable
			 {
		
		// TODO: do special things for reason=kill ?
		
		// System.err.println("sending exit msg to self "+this);
		mbox.put(ETuple.make(ERT.am_EXIT, from, reason));
	}

	/* (non-Javadoc)
	 * @see erjang.ETask#send_exit_to_all_linked(erjang.EObject)
	 */
	@Override
	protected void do_proc_termination(EObject result) throws Pausable {
		
		if (this.reply_closed_to != null) {
			this.reply_closed_to.send(self_handle(), ETuple.make(self_handle(), am_closed));
		}
		
		super.do_proc_termination(result);
		if (result != am_normal) {
			owner.send(self_handle(), ETuple.make(ERT.am_EXIT, self_handle(), result));
		}
		//this.port.done();
		all_ports.remove(this.id);
		
		EDriverControl i = instance;
		if (i != null) 
			i.stop(result);
		
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see erjang.driver.IOHandler#ready(java.nio.channels.SelectableChannel,
	 * int)
	 */
	@Override
	public void ready(final SelectableChannel ch, final int readyOps) {
		mbox.putb(new EPortControl() {
			@Override
			public void execute() throws Pausable {
				if ((readyOps & EDriverInstance.ERL_DRV_READ) == EDriverInstance.ERL_DRV_READ) {
					instance.readyInput(ch);
				}
				if ((readyOps & EDriverInstance.ERL_DRV_WRITE) == EDriverInstance.ERL_DRV_WRITE) {
					instance.readyOutput(ch);
				}
				if ((readyOps & EDriverInstance.ERL_DRV_CONNECT) == EDriverInstance.ERL_DRV_CONNECT) {
					instance.readyConnect(ch);
				}
				if ((readyOps & EDriverInstance.ERL_DRV_ACCEPT) == EDriverInstance.ERL_DRV_ACCEPT) {
					instance.readyAccept(ch);
				}
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * erjang.driver.IOHandler#released(java.nio.channels.SelectableChannel)
	 */
	@Override
	public void released(final SelectableChannel ch) {
		mbox.putb(new EPortControl() {
			@Override
			public void execute() throws Pausable {
				instance.stopSelect(ch);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * erjang.driver.IOHandler#exception(java.nio.channels.SelectableChannel,
	 * java.io.IOException)
	 */
	@Override
	public void exception(SelectableChannel ch, IOException e) {
		// TODO Auto-generated method stub

	}

	/**
	 * @param howlong
	 */
	public void set_timer(long howlong) {
		this.abs_timeout = System.currentTimeMillis() + howlong;
	}
	
	public long read_timer() {
		return this.abs_timeout - System.currentTimeMillis();
	}
	
	/**
	 * @param port2
	 */
	public void cancel_timer(EPort port2) {
		assert port2 == this.port : "can only cancel timer on self" ;
		this.abs_timeout = 0;
	}


	/**
	 * @param job
	 */
	public void async_done(final EAsync job) {

		mbox.putb(new EPortControl() {
			@Override
			public void execute() throws Pausable {
				instance.readyAsync(job);
			}
		});
		}

	/**
	 * @param out
	 * @throws Pausable 
	 */
	public void output_from_driver(EObject out) throws Pausable {
		output_term_from_driver(new ETuple2(port, new ETuple2(am_data, out)));
	}

	public void output_from_driver_b(EObject out) {
		output_term_from_driver_b(new ETuple2(port, new ETuple2(am_data, out)));
	}

	public void output_term_from_driver(EObject out) throws Pausable {
		if (ERT.DEBUG_PORT) System.err.println(""+owner+" ! "+out);
		owner.send(port, out);
	}

	public void output_term_from_driver_b(EObject out) {
		if (ERT.DEBUG_PORT) System.err.println(""+owner+" ! "+out);
		owner.sendb(out);
	}

	/**
	 * @throws Pausable 
	 * 
	 */
	public void eof_from_driver_b() {
		output_term_from_driver_b(new ETuple2(port, am_eof));
	}

	public void eof_from_driver() throws Pausable {
		output_term_from_driver(new ETuple2(port, am_eof));
	}

	public void exit_status_from_driver(int code) throws Pausable {
		output_term_from_driver(new ETuple2(port, ERT.box(code)));
	}

	public void exit_status_from_driver_b(int code) {
		output_term_from_driver_b(new ETuple2(port, ERT.box(code)));
	}

	public static ESeq all_ports() {
		
		ESeq res = ERT.NIL;
		for (EDriverTask dt : all_ports.values()) {
			if (dt.isDone()) continue;
			res = res.cons(dt.self_handle());
		}

		return res;
	}

	public EObject port_info(EAtom spec) {
		
		if (spec == am_connected) {
			return new ETuple2(am_connected, owner);
		}
		
		if (spec == am_name) {
			return new ETuple2(am_name, getName());
		}
		
		if (spec == EProc.am_links) {
			return new ETuple2(EProc.am_links, links());
		}
		
		throw new NotImplemented("port_info(" + spec + ")");
	 
	}

	public void exit(final EObject reason) {
		mbox.putb(new EPortControl() {
			@Override
			public void execute() throws Pausable, IOException {
				throw new ErlangExit(reason);
			}
		});
	}

	public EPeer node() {
		return this.peer;
	}

	public void node(EPeer peer) {
		this.peer = peer;
		this.status |= ERTS_PORT_SFLG_DISTRIBUTION;
	}

	/** magic direct call ! */
	public void outputv(EHandle sender, ByteBuffer[] ev) throws IOException, Pausable {
		this.command(sender, ev);
	}

	public boolean send_binary_data() {
		return send_binary_data;
	}


}
