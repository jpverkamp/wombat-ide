/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text.sta;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Scanner;

/**
 * Implements a shared text area server. Only one of this will exist per shared
 * text area group. It is responsible for relaying messages between the connected
 * clients and controlling who currently has the edit token.
 * 
 * Whichever instance of Wombat is hosting a SharedTextArea will have both this 
 * server running and their own client.
 */
public class Server {
	public static final int TCP_PORT = 5310;
	
	// Set this to false to stop all threads.
	public boolean Running = true;
	
	// The name that the server uses to refer to itself. Should be unique. 
	String Name;
	String IP;
	
	// All of the clients that are connected to this server.
	List<ClientThread> Clients; 
	
	// Which client currently has the edit token. -1 for no clients.
	int CurrentEditToken = -1;
	
	// Create a new server.
	public Server(String name) throws IOException {
		// Set identifying information for this server.
		Name = name;
		IP = InetAddress.getLocalHost().getHostAddress();
		
		// Start the threads.
		new MulticastServerThread(this);
		new ServerThread(this);
	}

	/**
	 * A client has just returned the token. Advance to the next client.
	 */
	public void advanceToken() {
		// Special case for no current clients.
		if (Clients.size() == 0)
			CurrentEditToken = -1;
		
		// Advance the token and notify that client.
		CurrentEditToken = (CurrentEditToken + 1) % Clients.size();
		Clients.get(CurrentEditToken).send("token");
	}

	/**
	 * Relay a message to all clients other than the original source.
	 * @param msg The message.
	 * @param src The original source.
	 */
	public void relayMessage(String msg, ClientThread src) {
		// Ignore messages from clients that don't have the token.
		if (Clients.get(CurrentEditToken) != src)
			return;
		
		// Otherwise, relay to all other clients.
		for (ClientThread c : Clients)
			if (c != src)
				c.send(msg);
	}
}

/**
 * Listen for incoming multicast connections.
 */
class MulticastServerThread extends MulticastThread {
	Server S;
	
	/**
	 * Create a new runnable.
	 * @param s The server we are associated with.
	 * @throws IOException If we cannot connect to the multicast.
	 * @throws UnknownHostException If we cannot figure out who to talk to.
	 */
	public MulticastServerThread(Server s) throws UnknownHostException, IOException {
		super("STA multicast server thread");
		
		S = s;
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Main loop for the multicast thread. Just start listening and keep going.
	 */
	public void run() {
		try {
			// Say hi to any clients that already exist.
			multicastSend("server," + S.Name + "," + S.IP);

			// Listen. Forever. Mwahaha.
			while (S.Running) {
				String msg = multicastReceive();
					
				if (msg.startsWith("client")) {
					multicastSend("server," + S.Name + "," + S.IP);
				}
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
}

/**
 * Thread to process incoming connections from clients.
 */
class ServerThread extends Thread {
	Server S;
	
	/**
	 * Create a new runnable.
	 * @param s The server we are associated with.
	 */
	public ServerThread(Server s) {
		super("MTA server thread");
		
		S = s;
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Listen for incoming clients. 
	 */
	@Override public void run() {
		try {
			ServerSocket server = new ServerSocket(Server.TCP_PORT);
			
			while (S.Running) {
				new ClientThread(S, server.accept());
			}
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
}

/**
 * Thread to process incoming connections from clients.
 */
class ClientThread extends Thread {
	Server S;
	Socket C;
	
	Scanner FromClient;
	PrintWriter ToClient;
	
	/**
	 * Create a new runnable.
	 * @param s The server we are associated with.
	 * @param c The client we are listening to.
	 */
	public ClientThread(Server s, Socket c) {
		super("MTA client thread: " + c.getInetAddress().getHostAddress());
		
		S = s;
		C = c;
		
		try {
			FromClient = new Scanner(c.getInputStream());
			ToClient = new PrintWriter(c.getOutputStream());
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Send a message to this client. Likely either the token or a relayed message.
	 * @param msg The message to send.
	 */
	public void send(String msg) {
		ToClient.println(msg);
		ToClient.flush();
	}

	/**
	 * Listen for incoming clients. 
	 */
	@Override public void run() {
		// Read input from clients.
		while (S.Running && FromClient.hasNextLine()) {
			String msg = FromClient.nextLine();
			
			// If we're getting a token back, we need to advance to the next client.
			if (msg.startsWith("token")) {
				S.advanceToken();
			}
			
			// Otherwise, we need to relay the message to everyone else.
			else {
				S.relayMessage(msg, this);
			}
		}
	}
}