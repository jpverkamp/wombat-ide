package wombat.gui.text;

import java.awt.Color;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.event.*;

import wombat.util.Base64;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	String ID;
	boolean Running = true;
	
	// Used for the hosting text areas.
	ServerSocket Host = null;
	List<Client> Clients = new ArrayList<Client>();
	
	// Used for joining text areas.
	Client Server = null;
	
	// Store the last insert/remove.
	boolean lastWasInsert = false;
	int lastOffset = -1;
	int lastLength = -1;
	String lastString = "";
	
	/**
	 * Create a new text area.
	 */
	private SharedTextArea() {
		super();
		code.setBackground(new Color(255, 240, 240));
	}
	
	/**
	 * Host a new shared text area.
	 * @return The area.
	 * @throws Exception If we cannot host.
	 */
	public static SharedTextArea host() throws Exception {
		final SharedTextArea sta = new SharedTextArea();
		
		// Create the host.
		for (int i = 5309; ; i++) {
			try {
				sta.Host = new ServerSocket(i);
				break;
			} catch (IOException e) {
				continue;
			}
		}
		
		// Use that to determine the ID.
		sta.ID = InetAddress.getLocalHost().getHostAddress() + ":" + sta.Host.getLocalPort();
		
		// Start a thread to get new clients.
		Thread serverAcceptThread = new Thread() {
			public void run() {
				while (sta.Running)
					try {
						
						Client c = new Client(sta.Host.accept());
						c.send("hello");
						sta.Clients.add(c);

					} catch (IOException e) {
						e.printStackTrace();
					}
			}
		};
		serverAcceptThread.setDaemon(true);
		serverAcceptThread.start();
		
		// And another thread to process input from clients, copy and echo to other clients.
		Thread serverProcessThread = new Thread() {
			public void run() {
				String line;
				
				while (sta.Running) {
					try { Thread.sleep(50); } catch(InterruptedException ex) {}
					
					for (Client c : sta.Clients) {
						line = c.recv();
						if (line == null) continue;
						sta.processLocal(line);

						// Echo to other client
						for (Client cto : sta.Clients) 
							if (c != cto)
								cto.send(line);
					}
				}
			}
		};
		serverProcessThread.setDaemon(true);
		serverProcessThread.start();
		
		sta.code.getDocument().addDocumentListener(new NetworkedDocumentListener(sta));
		
		return sta;
	}

	/**
	 * Create a new text area connected to a given target.
	 * @param connectTo IP:Port
	 * @throws Exception If we can't get the server.
	 */
	public static SharedTextArea join(String connectTo) throws Exception {
		String[] parts = connectTo.split(":");
		InetAddress ip = InetAddress.getByName(parts[0]);
		int port = Integer.parseInt(parts[1]);
		
		final SharedTextArea sta = new SharedTextArea();
		sta.ID = connectTo;
		
		// Create a socket for it.
		sta.Server = new Client(new Socket(ip, port));
		sta.Server.send("hello");
		
		// Set up a listening thread.
		Thread fromServerThread = new Thread() {
			public void run() {
				while (sta.Running) {
					String line = sta.Server.recv();
					if (line != null)
						sta.processLocal(line);
				}
			}
		};
		fromServerThread.setDaemon(true);
		fromServerThread.start();

		sta.code.getDocument().addDocumentListener(new NetworkedDocumentListener(sta));
		
		return sta;
	}
	
	/**
	 * Clamp a number to a given range.
	 * @param lo Low bound.
	 * @param x Clamp this.
	 * @param hi High bound.
	 * @return The number clamped.
	 */
	int clamp(int lo, int x, int hi) {
		if (x < lo)
			return lo;
		else if (x > hi)
			return hi;
		else
			return x;
	}
	
	/**
	 * Process a line locally, either as a host or when joined.
	 * @param line The new line input.
	 */
	protected void processLocal(String line) {
		String[] parts = line.split(",");
		
		try {
			if ("insert".equals(parts[0])) {
				
				int off = Integer.parseInt(parts[1]);
				String str = new String(Base64.decode(parts[3]));
				
				if (lastWasInsert && off == lastOffset && lastString.equals(parts[3])) return;
				
				code.getDocument().insertString(off, str, null);
				
			} else if ("remove".equals(parts[0])) {
				
				int off = Integer.parseInt(parts[1]);
				int len = Integer.parseInt(parts[2]);
				
				if (!lastWasInsert && off == lastOffset && len == lastLength) return;
				
				code.getDocument().remove(off, len);
				
			}
		} catch(Exception ex) {
			ex.printStackTrace();
		}
	}

	/**
	 * ID accessor.
	 * @return The ID.
	 */
	public String getID() {
		return ID;
	}
}

/**
 * Helper class to store all information about clients.
 */
class Client {
	private Socket Socket;
	private Scanner From;
	private PrintWriter To;
	
	/**
	 * Connect a client to a socket.
	 * @param socket The socket.
	 * @throws IOException If we cannot connect.
	 */
	public Client(Socket socket) throws IOException {
		Socket = socket;
		From = new Scanner(socket.getInputStream());
		To = new PrintWriter(socket.getOutputStream());
	}
	
	/**
	 * Shut down the client
	 */
	public void dispose() {
		try {
			To.close();
			From.close();
			Socket.close();
		} catch (IOException e) {
		}
	}
	
	/**
	 * Send a line to the client
	 * @param msg
	 */
	public void send(String msg) {
		System.out.println("send: " + msg); // debug
		To.println(msg); 
		To.flush();
	}
	
	/**
	 * Next line from the client or null.
	 * @return Read
	 */
	public String recv() {
		if (From.hasNextLine()) {
			String msg = From.nextLine();
			System.out.println("recv: " + msg); // debug
			return msg;
		} else {
			return null;
		}
	}
}

/**
 * Document listener which forwards changes to the network.
 */
class NetworkedDocumentListener implements DocumentListener {
	SharedTextArea STA;

	/**
	 * Create a new networked document listener.
	 * @param sta The shared text area.
	 */
	public NetworkedDocumentListener(SharedTextArea sta) {
		STA = sta;
	}
	
	/**
	 * When the document has changed. (Formatting, ignore these.)
	 */
	@Override public void changedUpdate(DocumentEvent event) { }

	/**
	 * When something is inserted.
	 */
	@Override public void insertUpdate(DocumentEvent event) {
		try {
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes());
			
			String msg = "insert," + off + "," + len + "," + str;
			
			if (STA.Server != null) 
				STA.Server.send(msg);
			
			if (STA.Clients != null) 
				for (Client c : STA.Clients)
					c.send(msg);
			
			STA.lastWasInsert = true;
			STA.lastOffset = off;
			STA.lastString = str;

		} catch(Exception e) {
		}
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
		int off = event.getOffset();
		int len = event.getLength();
		
		String msg = "remove," + off + "," + len;
				
		if (STA.Server != null) 
			STA.Server.send(msg);
		
		if (STA.Clients != null) 
			for (Client c : STA.Clients)
				c.send(msg);
		
		STA.lastWasInsert = false;
		STA.lastOffset = off;
		STA.lastLength = len;
	}
}