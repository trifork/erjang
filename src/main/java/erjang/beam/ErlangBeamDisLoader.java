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

package erjang.beam;

import java.io.File;
import java.io.IOException;

import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import erjang.ETuple;

public class ErlangBeamDisLoader extends BeamLoader {

	String myid = "JVM2@localhost";
	OtpConnection conn;
	private OtpSelf self;
	private OtpPeer peer;

	// {'$gen_call', {To, Tag}, {call, Mod, Fun, Args, User}}
	
	public ErlangBeamDisLoader() throws OtpAuthException, IOException {
		self = new OtpSelf(myid);
		peer = new OtpPeer("beam_loader@localhost");
		conn = self.connect(peer);
		
		System.out.println("connexted to " + peer);
	}
	
	@Override
	public BeamFileData load(File file) throws IOException {

		sendGEN(conn, "beam_loader", new OtpErlangTuple(
				new OtpErlangObject[] { 
							new OtpErlangAtom("disasm"), 
							new OtpErlangString(file.getAbsolutePath()) }));

		try {
			OtpErlangObject reply = conn.receiveRPC();

			return new BeamFileData((ETuple)OtpConverter.convert(reply));

		} catch (OtpErlangExit e) {
			throw new RuntimeException("external beam_loader died", e);
		} catch (OtpAuthException e) {
			throw new RuntimeException("external beam_loader auth", e);
		}

	}

	@Override
	public BeamFileData load(byte[] data) throws IOException {

		sendGEN(conn, "beam_loader", new OtpErlangTuple(
				new OtpErlangObject[] { 
							new OtpErlangAtom("disasm"),
							new OtpErlangBinary(data) }));

		try {
			OtpErlangObject reply = conn.receiveRPC();

			return new BeamFileData((ETuple)OtpConverter.convert(reply));

		} catch (OtpErlangExit e) {
			throw new RuntimeException("external beam_loader died", e);
		} catch (OtpAuthException e) {
			throw new RuntimeException("external beam_loader auth", e);
		}

	}

	public void sendGEN(final OtpConnection conn, String server, final OtpErlangObject request) throws IOException {
		final OtpErlangObject[] gen = new OtpErlangObject[3];
		final OtpErlangObject[] reply = new OtpErlangObject[2];

		/* {self, { call, Mod, Fun, Args, user}} */

		reply[0] = self.pid();
		reply[1] = self.createRef();
		
		gen[0] = new OtpErlangAtom("$gen_call");
		gen[1] = new OtpErlangTuple(reply);
		gen[2] = request;

		conn.send(server, new OtpErlangTuple(gen));
	}

}
