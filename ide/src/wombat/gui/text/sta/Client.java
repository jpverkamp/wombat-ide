package wombat.gui.text.sta;

import java.io.IOException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;

/**
 * Implements the client part of a shared text area client. Used to establish
 * a connection with a server and relay any changes between the shared text 
 * area and the server and any changes on other clients back to the text area.
 */
public class Client {
	public static final int TOKEN_TIMEOUT = 100;
	
	// Set this to false to stop all threads.
	public boolean Running = true;
	
	// Remember all servers we see.
	Map<String, String> Servers = new HashMap<String, String>();
	
	/**
	 * Create a new client.
	 */
	public Client() {
		
	}
}

/**
 * Listen for incoming multicast connections.
 */
class MulticastClientThread extends MulticastThread {
	Client C;
	
	/**
	 * Create a new runnable.
	 * @param s The server we are associated with.
	 * @throws IOException If we cannot connect to the multicast.
	 * @throws UnknownHostException If we cannot figure out who to talk to.
	 */
	public MulticastClientThread(Client c) throws UnknownHostException, IOException {
		super("STA multicast server thread");
		
		C = c;
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Main loop for the multicast thread. Just start listening and keep going.
	 */
	public void run() {
		try {
			// Say hi to any servers that already exist.
			multicastSend("client");

			// Listen. Forever. Mwahaha.
			while (C.Running) {
				String msg = multicastReceive();
					
				// A server said hello. Remember it's name.
				if (msg.startsWith("server")) {
					String[] parts = msg.split(",");
					if (parts.length == 3)
						C.Servers.put(parts[0], parts[1]);
				}
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
}