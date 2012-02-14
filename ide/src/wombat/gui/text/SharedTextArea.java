package wombat.gui.text;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.text.BadLocationException;

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
		
		// Set up a listener to send out typed characters.
		sta.code.addKeyListener(new KeyListener() {
			@Override public void keyTyped(KeyEvent ke) {
				String send = "type," + sta.code.getCaretPosition() + "," + (int) ke.getKeyChar();
				for (Client c : sta.Clients)
					c.send(send);
			}
			
			@Override public void keyReleased(KeyEvent arg0) {}
			
			@Override public void keyPressed(KeyEvent arg0) {}
		});
		
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
		
		// Set up a listener to send out typed characters.
		sta.code.addKeyListener(new KeyListener() {
			@Override public void keyTyped(KeyEvent ke) {
				sta.Server.send("type," + sta.code.getCaretPosition() + "," + (int) ke.getKeyChar());					
			}
			
			@Override public void keyReleased(KeyEvent arg0) {}
			
			@Override public void keyPressed(KeyEvent arg0) {}
		});
		
		return sta;
	}
	
	/**
	 * Process a line locally, either as a host or when joined.
	 * @param line The new line input.
	 */
	protected void processLocal(String line) {
		String[] parts = line.split(",");
		if (parts.length == 2) {
			if (!"type".equals(parts[0])) return;
			
			int at = Integer.parseInt(parts[1]);
			char c = (char) Integer.parseInt(parts[2]);
			
			try {
				code.getDocument().insertString(at, "" + c, null);
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
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