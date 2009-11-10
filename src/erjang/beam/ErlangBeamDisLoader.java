package erjang.beam;

import java.io.File;
import java.io.IOException;


import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

import erjang.ETuple;
import erjang.beam.analysis.BeamTypeAnalysis;

public class ErlangBeamDisLoader extends BeamLoader {

	String myid = "JVM@localhost";
	OtpConnection conn;
	private OtpSelf self;
	private OtpPeer peer;

	// {'$gen_call', {To, Tag}, {call, Mod, Fun, Args, User}}
	
	ErlangBeamDisLoader() throws OtpAuthException, IOException {
		self = new OtpSelf(myid);
		peer = new OtpPeer("beam_loader@localhost");
		conn = self.connect(peer);

		
		
		System.out.println("connexted to " + peer);
	}

	@Override
	BeamFile load(File file) throws IOException {

		sendGEN(conn, "beam_loader", new OtpErlangTuple(
				new OtpErlangObject[] { 
							new OtpErlangAtom("disasm"), 
							new OtpErlangString(file.getAbsolutePath()) }));

		try {
			OtpErlangObject reply = conn.receiveRPC();

			return new BeamFile((ETuple)OtpConverter.convert(reply));

		} catch (OtpErlangExit e) {
			throw new RuntimeException("external beam_loader died", e);
		} catch (OtpAuthException e) {
			throw new RuntimeException("external beam_loader auth", e);
		}

	}

	public static void main(String[] args) throws IOException, OtpAuthException {
		System.setProperty("OtpConnection.trace", "4");
		ErlangBeamDisLoader foo = new ErlangBeamDisLoader();
		BeamFile loaded = foo.load(new File("/Users/krab/Systems/otp_src_R13B02-1/lib/compiler/ebin/beam_disasm.beam"));
		loaded.accept(new BeamTypeAnalysis());
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
