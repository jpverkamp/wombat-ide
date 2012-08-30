/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.*;
import javax.swing.Timer;
import javax.swing.event.*;
import javax.swing.text.BadLocationException;

import wombat.util.Base64;
import wombat.util.NameGenerator;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	public static final boolean NETWORK_DEBUG = true;
	public static int NEXT_PORT = 5309;
	
	boolean Running = true;
	
	String DocumentName;
	
	ServerThread ST;
	ClientThread CT;
	
	NetworkedDocumentListener NDL;
	
	/**
	 * Create the shared text area.
	 * @param host The address of the server
	 * @param port The port to connect on
	 * @param server We should host the server
	 */
	public SharedTextArea(InetAddress host, int port, boolean server) {
		super(true, true);
		DocumentName = host.getHostAddress() + ":" + port;
		
		// Custom text pane that displays server information.
		code = new LinedTextPane(this);
        add(new JScrollPane(code));
        
        // Distinguish it from other text areas.
        code.setBackground(new Color(240, 255, 240));
        
        // Attach the document listener.
        NDL = new NetworkedDocumentListener(this);
        code.getDocument().addDocumentListener(NDL);
        
		// Create the server if requested, the client either way.
        try {
            if (server) {
            	ST = new ServerThread(this, port);
            	CT = new ClientThread(this, InetAddress.getLocalHost(), port);
            } else {
            	CT = new ClientThread(this, host, port);
            }        	
        } catch(Exception e) {
        	code.setText("Unable to establish connection: " + e);
        }

	}
	
	/**
	 * Process a received line.
	 * @param line The new line input.
	 * @param onServer If we're doing something on the host, versus on a join'er.
	 * @return Any response to be sent back.
	 */
	protected synchronized void processLocal(String line) {
		NDL.Suppress = true;
		
		try {
			String[] parts = line.split(",");
			String lineMsgType = parts[0];
			String[] args = Arrays.copyOfRange(parts, 1, parts.length); 
			
			// Text has been inserted into the remote document.
			if ("insert".equals(lineMsgType)) {
				
				int off = Integer.parseInt(args[0]);
				String str = new String(Base64.decode(args[2]), "UTF-8");
				code.getDocument().insertString(off, str, null);
				
			}
			
			// Text has been removed from the remote document.
			else if ("remove".equals(lineMsgType)) {
				
				int off = Integer.parseInt(args[0]);
				int len = Integer.parseInt(args[1]);
				code.getDocument().remove(off, len);
				
			}
			
			// Someone has decided to say hello, send them our document
			else if ("hello".equals(lineMsgType)) {
				
				String str = Base64.encodeBytes(code.getText().getBytes("UTF-8"));
				CT.send(makeMessage("sync", str));
				
			}
			
			// We have a sync request, honor it
			else if ("sync".equals(lineMsgType)) {
				
				try {
					String str = new String(Base64.decode(args[0]), "UTF-8");
					code.setText(str);
				} catch(Exception e) {
				}
				
			}
			
			// Someone wants to check to see if we're in sync
			else if ("check-sync".equals(lineMsgType)) {
				
				try {
					int usHash = code.getText().hashCode();
					int themHash = Integer.parseInt(args[0]);
					
					if (usHash != themHash) {
						String str = Base64.encodeBytes(code.getText().getBytes("UTF-8"));
						CT.send(makeMessage("sync", str));
					}
					
				} catch(Exception e) {
				}
				
			}
			
		} catch(BadLocationException ex) {
			ex.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		NDL.Suppress = false;
	}

	/**
	 * Make a message to send over the network.
	 * @param cmd The command to send.
	 * @param args Any necessary arguments.
	 * @return
	 */
	public String makeMessage(String cmd, Object ... args) {
		StringBuilder msg = new StringBuilder();
		msg.append(cmd);
		for (Object o : args) {
			msg.append(',');
			msg.append(o.toString());
		}
		return msg.toString();
	}

	/**
	 * Get the document name.
	 * @return Duh.
	 */
	public String getDocumentName() {
		return DocumentName;
	}
}

/**
 * Document listener which forwards changes to the network.
 */
class NetworkedDocumentListener implements DocumentListener {
	SharedTextArea STA;
	boolean Suppress = false;
	Timer T;
	
	/**
	 * Create a new networked document listener.
	 * @param sta The shared text area.
	 */
	public NetworkedDocumentListener(SharedTextArea sta) {
		STA = sta;
		T = new Timer(1000, new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				int hash = STA.code.getText().hashCode();
				STA.CT.send(STA.makeMessage("check-sync", hash));
			}
		});
		T.setCoalesce(true);
		T.setRepeats(false);
		T.setDelay(Integer.MAX_VALUE);
		T.start();
	}
	
	/**
	 * When the document has changed. (Formatting, ignore these.)
	 */
	@Override public void changedUpdate(DocumentEvent event) { }

	/**
	 * When something is inserted.
	 */
	@Override public void insertUpdate(DocumentEvent event) {
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes("UTF-8"));
			
			STA.CT.send(STA.makeMessage("insert", off, len, str));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		T.restart();
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			
			STA.CT.send(STA.makeMessage("remove", off, len));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		T.restart();
	}
}

/**
 * Act as a server, basically just relaying messages between clients.
 */
class ServerThread extends Thread {
	SharedTextArea STA;
	
	ServerSocket Server;
	List<ServerClientThread> Clients;
	
	
	/**
	 * Create a new server.
	 * @param sta The shared text area that created this thread.
	 * @param port  
	 */
	public ServerThread(SharedTextArea sta, int port) {
		STA = sta;
		
		try {
			Server = new ServerSocket(port);
			Clients = new ArrayList<ServerClientThread>();
			
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
				Clients.add(new ServerClientThread(this, Server.accept()));
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
	public void relay(ServerClientThread sct, String msg) {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("ST relay: " + msg);
		
		for (ServerClientThread ea : Clients)
			if (ea != sct) 
				ea.send(msg);
	}
}


/**
 * Thread that runs on the server to keep track of any connected clients.
 */
class ServerClientThread extends Thread {
	ServerThread ST;
	Socket C;
	
	PrintWriter ToClient;
	Scanner FromClient;
	
	/**
	 * Create a new server client thread.
	 * @param st
	 */
	public ServerClientThread(ServerThread st, Socket c) {
		ST = st;
		C = c; 
		
		try {
			ToClient = new PrintWriter(c.getOutputStream());
			FromClient = new Scanner(c.getInputStream());
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Send a message out to this client.
	 * @param msg The message to send.
	 */
	public void send(String msg) {
		if (SharedTextArea.NETWORK_DEBUG) {
			int i = 0;
			for (ServerClientThread ea : ST.Clients) {
				if (this == ea) 
					System.out.println("SCT:" + i + " send: " + msg);
				i++;
			}
		}
		
		ToClient.println(msg);
		ToClient.flush();
	}

	/**
	 * Run the server client thread.
	 */
	public void run() {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("SCT running");
		
		while (ST.STA.Running) {
			if (FromClient.hasNextLine()) {
				String msg = FromClient.nextLine();
				
				if (SharedTextArea.NETWORK_DEBUG) {
					int i = 0;
					for (ServerClientThread ea : ST.Clients) {
						if (this == ea) 
							System.out.println("SCT:" + i + " recv: " + msg);
						i++;
					}
				}
				
				ST.relay(this, msg);
			}
		}
		
		try {
			ToClient.close();
			FromClient.close();
			C.close();
		} catch(Exception e) {
		}
		
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("SCT stopping");
	}
}

/**
 * Client thread. Sends local changes and receives remote changes. 
 */
class ClientThread extends Thread {
	SharedTextArea STA;
	Socket S;
	PrintWriter ToServer;
	Scanner FromServer;
	String MyName;
	
	/**
	 * Create a new client.
	 * @param sta The shared text area that created this client.
	 * @param host The IP of the server to connect to
	 * @param port The port of the server to connect to
	 */
	public ClientThread(SharedTextArea sta, InetAddress host, int port) {
		STA = sta;
		MyName = NameGenerator.getName();
		
		try {
			S = new Socket(host, port);
			ToServer = new PrintWriter(S.getOutputStream());
			FromServer = new Scanner(S.getInputStream());
		} catch (IOException e) {
			STA.code.setText("Unable to connect to client: " + e);
		}
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Send a message to this client's server.
	 * @param msg
	 */
	public void send(String msg) {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("CT:" + MyName + " send: " + msg);
		
		ToServer.println(msg);
		ToServer.flush();
	}

	/**
	 * Run the client.
	 */
	public void run() {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("CT '" + MyName + "' running");
		
		send(STA.makeMessage("hello"));
		
		while (STA.Running) {
			if (FromServer.hasNextLine()) {
				String msg = FromServer.nextLine();
				
				if (SharedTextArea.NETWORK_DEBUG)
					System.out.println("CT:" + MyName + " recv: " + msg);
				
				STA.processLocal(msg);
			}
		}
		
		try {
			ToServer.close();
			FromServer.close();
			S.close();
		} catch(Exception e) {
		}
		
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("CT '" + MyName + "' stopping");
	}
}