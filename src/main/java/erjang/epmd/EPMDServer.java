package erjang.epmd;

import java.nio.channels.SelectionKey;

public class EPMDServer extends PacketServer {

	private static int epmd_port;

	@Override
	protected PacketConnection newConnection(SelectionKey dsk) {

		return new EPMDConnection(epmd_port, dsk);
		
	}

	static public void main(String[] args) {
		epmd_port = 4369;
		int level = 2;
		for (int i = 0; i < args.length; i++) {
			if (args[i].startsWith("-p"))
				epmd_port = Functions.toInt(args[i].substring(2), epmd_port);
			if (args[i].startsWith("-d"))
				level = Functions.toInt(args[i].substring(2), level);
		}
		Functions.setDebugLevel(level);
		new EPMDServer().listen(epmd_port);
	}


}
