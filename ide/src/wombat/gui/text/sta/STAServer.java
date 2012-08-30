package wombat.gui.text.sta;

import java.io.IOException;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.List;

/**
 * Act as a server, basically just relaying messages between clients.
 */
public class STAServer extends Thread {
	SharedTextArea STA;
	
	ServerSocket Server;
	List<STAServerConnection> Clients;
	
	
	/**
	 * Create a new server.
	 * @param sta The shared text area that created this thread.
	 * @param port  
	 */
	public STAServer(SharedTextArea sta, int port) {
		STA = sta;
		
		try {
			Server = new ServerSocket(port);
			Clients = new ArrayList<STAServerConnection>();
			
			setDaemon(true);
			start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Run the server.
	 */
	public void run() {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("ST running");
		
		while (STA.Running) {
			try {
				Clients.add(new STAServerConnection(this, Server.accept()));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("ST stopping");
	}

	/**
	 * Relay a message to any other connected clients.
	 * @param sct The thread that the message came from
	 * @param msg The message to relay
	 */
	public void relay(STAServerConnection sct, String msg) {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("ST relay: " + msg);
		
		for (STAServerConnection ea : Clients)
			if (ea != sct) 
				ea.send(msg);
	}
}