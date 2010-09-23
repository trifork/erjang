/**
 * This file is part of Erjang - A JVM-based Erlang VM
 *
 * Copyright (c) 2010 by Trifork
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

package erjang.driver.inet_gethost;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectableChannel;

import kilim.Pausable;
import erjang.EHandle;
import erjang.EString;
import erjang.NotImplemented;
import erjang.driver.EAsync;
import erjang.driver.EDriver;
import erjang.driver.EDriverInstance;
import erjang.driver.IO;

public class GetHostDriver extends EDriverInstance {

	public static final byte OP_GETHOSTBYNAME = 1;
	public static final byte OP_GETHOSTBYADDR = 2;
	public static final byte OP_CANCEL_REQUEST = 3;
	public static final byte OP_CONTROL = 4;

	public static final byte PROTO_IPV4 = 1;
	public static final byte PROTO_IPV6 = 2;

	public static final byte SETOPT_DEBUG_LEVEL = 0;

	public static final byte UNIT_ERROR = 0;
	public static final byte UNIT_IPV4 = 4;
	public static final byte UNIT_IPV6 = 16;

	
	private EString command;

	public GetHostDriver(EDriver driver, EString command) {
		super(driver);
		this.command = command;
	}

	@Override
	protected void flush() throws Pausable {

	}

	@Override
	protected void output(EHandle caller, ByteBuffer buf) throws IOException,
			Pausable {

		// System.err.println(EBinary.make(buf));
		
		final int seq = buf.getInt();
		byte op = buf.get();
		EAsync eas;
		
		switch (op) {
		case OP_GETHOSTBYNAME: {
			eas = gethostbyname(buf, seq);
			break;
		}

		case OP_GETHOSTBYADDR: {
			eas = gethostbyaddr(buf, seq);
			break;
		}

		default:
			throw new NotImplemented("inet_gethost seq="+seq+"; op="+op);

		}
		
		
		driver_async(eas);
	}

	private EAsync gethostbyname(ByteBuffer buf, final int seq) {
		final byte proto = buf.get();
		final String host = IO.getstr(buf, true);
		// System.err.println(" gethostsbyname["+seq+"][call] "+host);

		return new EAsync() {
			
			InetAddress[] addr;
			String[] names;
			private UnknownHostException err;
			InetAddress primary;
			
			@Override
			public void async() {
				
				// System.err.println(" gethostsbyname["+seq+"][async] "+host);
				try {
					primary = InetAddress.getByName(host);
					addr = InetAddress.getAllByName(host);
					names = new String[addr.length];
					for (int i = 0; i < addr.length; i++) {
						names[i] = addr[i].getCanonicalHostName();
						// System.err.println(">> " + addr[i]+" -> "+names[i]);
					}
				} catch (UnknownHostException e) {
					this.err = e;
				}
			}

			@Override
			public void ready() throws Pausable {
				
				// System.err.println(" gethostsbyname["+seq+"][ready] "+host);
				if (addr != null) {
					// we're ok
					byte[][] bytes = new byte[addr.length][];
					
					int size = 4 + 1 + 4;
					
					int first = -1;
					int acount = 0;
					for (int i = 0; i < addr.length; i++) {
						//System.err.println("gethostbyname["+i+"]="+addr[i]+" / "+names[i]);
						if ((proto==PROTO_IPV4 && (addr[i] instanceof Inet4Address))
								|| (proto==PROTO_IPV6 && (addr[i] instanceof Inet6Address))) {
							bytes[i] = addr[i].getAddress();
							size += bytes[i].length;		
							acount += 1;
							if (first == -1) first = i;
							if (host.equals(names[i])) {
								//System.err.println("gethostbyname["+i+"]=>"+primary);
								first = i;
							}
						}
					}
					
					size += 4;
					size += host.length() + 1;
					
					ByteBuffer rep = ByteBuffer.allocate(size);
					
					rep.putInt(seq);
					
					if (proto == PROTO_IPV4) {
						rep.put(UNIT_IPV4);
						rep.putInt(acount);
						if (acount > 0) {
							rep.put(bytes[first]);
							for (int i = 0; i < addr.length; i++) {
								if (i != first && bytes[i] != null) {
									rep.put(bytes[i]);
								}
							}
						}						
						rep.putInt(1);
						rep.put(host.getBytes(IO.ISO_LATIN_1));
						rep.put((byte)0);
						
						// dump_write(new ByteBuffer[] {rep});
						
						driver_output(rep);
						
						return;
						
					} else if (proto == PROTO_IPV6) {
						rep.put(UNIT_IPV6);
						rep.putInt(acount);
						for (int i = 0; i < addr.length; i++) {
							if (bytes[i] != null) {
								rep.put(bytes[i]);
							}
						}
						
						rep.putInt(1);
						rep.put(host.getBytes(IO.ISO_LATIN_1));
						rep.put((byte)0);
						
						// dump_write(new ByteBuffer[] {rep});
						driver_output(rep);
						return;
					}
				}
					
				String message = "unknown";
				
				if (err != null) {
					message = err.getMessage();
				}
					
					
				byte[] msg = message.getBytes(IO.ISO_LATIN_1);
				int len = 4 + 1 + msg.length;
				
				ByteBuffer rep = ByteBuffer.allocate(len);
				rep.putInt(seq);
				rep.put(UNIT_ERROR);
				rep.put(msg);
				
				dump_write(new ByteBuffer[] {rep});
				
				driver_output(rep);
			}
			
		};
	}


	private EAsync gethostbyaddr(ByteBuffer buf, final int seq) {
		final byte proto = buf.get();
		final byte[] host = new byte[ proto == PROTO_IPV4 ? 4 : 16 ];
		buf.get(host);
		
		// System.err.println(" gethostbyaddr["+seq+"][call] "+host);

		return new EAsync() {
			
			String name;
			private UnknownHostException err;
			InetAddress primary;
			InetAddress[] addr;
			
			@Override
			public void async() {
				
				// System.err.println(" gethostbyaddr["+seq+"][async] "+host);
				try {
					primary = InetAddress.getByAddress(host);
					name = primary.getCanonicalHostName();
					addr = InetAddress.getAllByName(name);
				} catch (UnknownHostException e) {
					this.err = e;
				}
			}

			@Override
			public void ready() throws Pausable {
				
				// System.err.println(" gethostsbyname["+seq+"][ready] "+host);
				if (addr != null) {
					// we're ok
					byte[][] bytes = new byte[addr.length][];
					
					int size = 4 + 1 + 4;
					
					int first = -1;
					int acount = 0;
					for (int i = 0; i < addr.length; i++) {
						//System.err.println("gethostbyname["+i+"]="+addr[i]+" / "+names[i]);
						if ((proto==PROTO_IPV4 && (addr[i] instanceof Inet4Address))
								|| (proto==PROTO_IPV6 && (addr[i] instanceof Inet6Address))) {
							bytes[i] = addr[i].getAddress();
							size += bytes[i].length;		
							acount += 1;
							if (first == -1) first = i;
						}
					}
					
					size += 4;
					size += name.length() + 1;
					
					ByteBuffer rep = ByteBuffer.allocate(size);
					
					rep.putInt(seq);
					
					if (proto == PROTO_IPV4) {
						rep.put(UNIT_IPV4);
						rep.putInt(acount);
						if (acount > 0) {
							rep.put(bytes[first]);
							for (int i = 0; i < addr.length; i++) {
								if (i != first && bytes[i] != null) {
									rep.put(bytes[i]);
								}
							}
						}						
						rep.putInt(1);
						rep.put(name.getBytes(IO.ISO_LATIN_1));
						rep.put((byte)0);
						
						// dump_write(new ByteBuffer[] {rep});
						
						driver_output(rep);
						
						return;
						
					} else if (proto == PROTO_IPV6) {
						rep.put(UNIT_IPV6);
						rep.putInt(acount);
						for (int i = 0; i < addr.length; i++) {
							if (bytes[i] != null) {
								rep.put(bytes[i]);
							}
						}
						
						rep.putInt(1);
						rep.put(name.getBytes(IO.ISO_LATIN_1));
						rep.put((byte)0);
						
						// dump_write(new ByteBuffer[] {rep});
						driver_output(rep);
						return;
					}
				}
					
				String message = "unknown";
				
				if (err != null) {
					message = err.getMessage();
				}
					
					
				byte[] msg = message.getBytes(IO.ISO_LATIN_1);
				int len = 4 + 1 + msg.length;
				
				ByteBuffer rep = ByteBuffer.allocate(len);
				rep.putInt(seq);
				rep.put(UNIT_ERROR);
				rep.put(msg);
				
				dump_write(new ByteBuffer[] {rep});
				
				driver_output(rep);
			}
			
		};
	}

	@Override
	protected void readyAsync(EAsync data) throws Pausable {
		data.ready();
	}

	@Override
	protected void readyInput(SelectableChannel ch) throws Pausable {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void readyOutput(SelectableChannel evt) throws Pausable {
		throw new erjang.NotImplemented();

	}

	@Override
	protected void timeout() throws Pausable {
		throw new erjang.NotImplemented();

	}

	public static void dump_write(ByteBuffer[] ev) {

		
		System.err.println(" vec[" + ev.length + "]:: ");

		for (int i = 0; i < ev.length; i++) {

			ByteBuffer evp = ev[i];
			int off = 0;
			for (int p = 0; p < evp.position(); p++) {

				if ((off % 0x10) == 0 && off != 0)
					System.err.println("");

				if ((off % 0x10) == 0)
					System.err.print("0x" + hex4(off) + " :");

				System.err.print(" ");
				byte ch = evp.get(p);
				System.err.print(hex2(ch&0xff));

				off += 1;

			}

			if (i < ev.length-1) System.out.println();
		}

		System.err.println("---");

		for (int i = 0; i < ev.length; i++) {

			ByteBuffer evp = ev[i];
			int off = 0;
			for (int p = 0; p < evp.position(); p++) {

				if ((off % 0x10) == 0 && off != 0)
					System.err.println("");

				if ((off % 0x10) == 0)
					System.err.print("0x" + hex4(off) + " : ");

				byte ch = evp.get(p);
				if (ch >= 32 && ch <= 127) {
					System.err.print((char)ch);
				} else {
					System.err.print('.');
				}

				off += 1;

			}

			if (i < ev.length-1) System.out.println();
		}


	}

	static String hex2(int i) {
		if (i < 0x10) return "0" + Integer.toHexString(i);
		return Integer.toHexString(i);
	}
	
	static String hex4(int i) {
		if (i < 0x10) return "000" + Integer.toHexString(i);
		if (i < 0x100) return "00" + Integer.toHexString(i);
		if (i < 0x1000) return "0" + Integer.toHexString(i);
		return Integer.toHexString(i);
	}
	

}
