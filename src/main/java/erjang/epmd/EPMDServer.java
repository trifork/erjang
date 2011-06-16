package erjang.epmd;

import java.nio.channels.SelectionKey;
import java.util.logging.Level;
import java.util.logging.Logger;

public class EPMDServer extends PacketServer {

	private static int epmd_port;

	@Override
	protected PacketConnection newConnection(SelectionKey dsk) {

		return new EPMDConnection(epmd_port, dsk);
		
	}

	static public void main(String[] args) {
		epmd_port = 4369;
		Level level = Level.WARNING;
		for (int i = 0; i < args.length; i++) {
			if (args[i].startsWith("-p"))
				epmd_port = toInt(args[i].substring(2), epmd_port);
			if (args[i].startsWith("-d"))
				level = toLevel(toInt(args[i].substring(2), 2));
		}
		final Logger log = Logger.getLogger("erjang.epmd");
		log.setLevel(level);
		
		new EPMDServer().listen(epmd_port);
	}
	
	public static int toInt(String intString, int defaultValue) {
		try {
			return Integer.parseInt(intString);
		} catch (NumberFormatException e) {
			return defaultValue;
		}
	}
	
	public static Level toLevel(int level) {
		if (level <= 0) {
			return Level.OFF;
		}
		switch (level) {
		case 1: return Level.SEVERE;
		case 2: return Level.WARNING;
		case 3: return Level.INFO;
		case 4: return Level.FINE;
		case 5: return Level.FINER;
		case 6: return Level.FINEST;
		default: return Level.FINEST;
		}
	}
}
