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

import kilim.Pausable;
import erjang.EAtom;
import erjang.EBinary;
import erjang.ECons;
import erjang.EHandle;
import erjang.EInternalPort;
import erjang.EObject;
import erjang.EPID;
import erjang.EPort;
import erjang.EProc;
import erjang.ERT;
import erjang.ESeq;
import erjang.EString;
import erjang.ETask;
import erjang.ETuple;
import erjang.ETuple2;
import erjang.ETuple3;
import erjang.ErlangError;
import erjang.ErlangException;
import erjang.ErlangExitSignal;
import erjang.NotImplemented;

/**
 * Base class for the two kinds of driver tasks: drivers, and "exec"s
 */
public abstract class EDriverTask extends ETask<EInternalPort> implements
		NIOHandler {

	@Override
	public String toString() {
		return "<driver_task:" + super.id + ">";
	}

	private static final EAtom am_data = EAtom.intern("data");
	private static final EAtom am_connected = EAtom.intern("connected");
	private final EInternalPort port;
	protected EPID owner;
	private final EDriverControl instance;

	
	private	static ConcurrentHashMap<Integer,EDriverTask> all_ports 
		= new ConcurrentHashMap<Integer,EDriverTask> ();

	protected EDriverTask(EProc owner, EDriverInstance driver) {
		this.owner = owner.self_handle();
		this.instance = driver;
		this.port = new EInternalPort(this);
		driver.task = this;
		

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

	/**
	 * @param cmd
	 * @param portSetting
	 */
	protected void parseOptions(ESeq command, EObject portSetting) {
		// TODO: most of this is way too expensive for non-exec ports

		// set by options
		this.cmd = new String[] { command.stringValue() };
		this.cwd = System.getProperty("user.dir");
		this.env = new HashMap<String, String>(System.getenv());

		this.packet = -1; // not set
		this.line_length = -1;

		this.send_exit_status = false;
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
	public void execute() throws Pausable {
		try {

			EObject result = null;
			try {
				this.pstate = State.RUNNING;

				// driver main loop
				main_loop();

				result = am_normal;

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
				this.pstate = State.DONE;
			}

			// System.err.println("task "+this+" exited with "+result);
			do_proc_termination(result);

			instance.stop();

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
			if ((t2 = ETuple2.cast(msg)) != null) {

				EObject sender = t2.elem1;

				ETuple2 cmd;
				if ((cmd = ETuple2.cast(t2.elem2)) != null) {
					// cmd must be one of
					// {command, iodata()}
					// {connect, PID}

					if (cmd.elem1 == EPort.am_command) {
						if (cmd.elem2.collectIOList(out)) {
							if (out.size() == 0) {
								instance.outputv(ERT.EMPTY_BYTEBUFFER_ARR);
							} else {
								instance.outputv(out.toArray(new ByteBuffer[out
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

						old_owner.send(ETuple.make(this.self_handle(),
								EPort.am_connected));

						continue next_message;

					}

				} else if (t2.elem2 == am_close) {
					// will call instance.stop()
					return;
				}

			} else if ((ctrl = msg.testPortControl()) != null) {

				// port control messages are simply run
				ctrl.execute();
				continue next_message;

			} else if ((t3 = ETuple3.cast(msg)) != null) {

				// {'EXIT', From, Reason} comes in this way
				if (t3.elem1 == ERT.EXIT) {
					// close is handled by exception handling code
					return;
				}
			}

			break;
		}

		throw new ErlangError(ERT.am_badsig, msg);
	}

	/**
	 * implementation of port_control
	 * 
	 * @param op
	 * @param out
	 */
	public EObject control(int op, ByteBuffer[] out) {

		if (pstate != State.RUNNING) {
			throw ERT.badarg();
		}

		ByteBuffer bb = instance.control(op, out);

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
	 */
	public EObject call(int op, EObject data) {
		if (pstate != State.RUNNING) {
			throw ERT.badarg();
		}

		EObject result = instance.call(op, data);

		if (result == null) {
			return ERT.NIL;
		} else {
			return result;
		}
	}

	/**
	 * erlang:port_command uses this, since error handling happens in the BIF
	 * 
	 * @param out
	 * @return
	 * @throws Pausable
	 */
	public void command(final ByteBuffer[] out) throws Pausable {

		if (mode != Mode.STREAM) {
			// do we need to encode the packet length here?
			// guess so, if we are non-stream mode
			throw new NotImplemented();
		}

		mbox.put(new EPortControl() {
			@Override
			public void execute() throws Pausable, IOException {
				instance.outputv(out);
			}
		});
	}

	/** our owner died, do something! */
	@Override
	protected void process_incoming_exit(EHandle from, EObject reason)
			throws Pausable {
		
		// TODO: do special things for reason=kill ?
		
		System.err.println("sending exit msg to self "+this);
		mbox.put(ETuple.make(ERT.EXIT, from, reason));
	}

	/* (non-Javadoc)
	 * @see erjang.ETask#send_exit_to_all_linked(erjang.EObject)
	 */
	@Override
	protected void do_proc_termination(EObject result) throws Pausable {
		super.do_proc_termination(result);
		if (result != am_normal) {
			owner.send(ETuple.make(ERT.EXIT, self_handle(), result));
		}
		//this.port.done();
		all_ports.remove(this.id);
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
	 */
	public void output_from_driver(EObject out) {
		owner.sendb(new ETuple2(port, new ETuple2(am_data, out)));
	}

	/**
	 * 
	 */
	public void eof_from_driver() {
		owner.sendb(new ETuple2(port, am_eof));
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
		
		throw new NotImplemented("port_info(" + spec + ")");
		
	}

}
