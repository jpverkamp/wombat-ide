package wombat.gui.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
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
import wombat.util.FixedLengthList;
import wombat.util.Options;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	static final boolean NETWORKING_DEBUG = false;
	static final char[][] BAD_CHARS = {
		{'l', '('}, {'1', ')'},
		{'o', '['}, {'O', ']'}, {'0', '*'}, 
		{'/', '?'},
	};
	
	String ID;
	boolean Running = true;
	SyncTimer SyncTimer;
	
	// Used for the hosting text areas.
	ServerSocket Host = null;
	List<Client> Clients = new ArrayList<Client>();
	
	// Used for joining text areas.
	Client Server = null;
	
	// Store the last insert/remove.
	FixedLengthList<String> lastInsertsAndRemoves = new FixedLengthList<String>(5);
	
	/**
	 * Create a new text area.
	 */
	private SharedTextArea() {
		super();
		
		// Custom text pane that displays server information.
		code = new LinedTextPane(this) {
			private static final long serialVersionUID = 7284878426815837099L;

			@Override public void paint(Graphics go) {
		    	super.paint(go);
		    	
		    	Graphics2D g = (Graphics2D) go;
		    	int width = 2 + 80 * g.getFontMetrics(new Font("Monospaced", Font.PLAIN, Options.FontSize)).charWidth(' '); 
		    	g.setColor(Color.LIGHT_GRAY);
		    	
		    	g.drawString(Host == null ? "Joined" : "Hosted", width + 10, 18);
		    	g.drawString(ID, width + 10, 36);
			}
		};
        add(new JScrollPane(code));
        
		code.setBackground(new Color(240, 255, 240));
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
		byte[] addr = InetAddress.getLocalHost().getAddress();
		byte[] data = Arrays.copyOf(addr, addr.length + 2);
		data[data.length - 2] = (byte) ((sta.Host.getLocalPort() / 256) - 128);
		data[data.length - 1] = (byte) ((sta.Host.getLocalPort() % 256) - 128);
		sta.ID = Base64.encodeBytes(data);
		
		// Convert bad characters
		for (char[] badPair : BAD_CHARS) 
			sta.ID = sta.ID.replace(badPair[0], badPair[1]);
		
		// Start a thread to get new clients.
		Thread serverAcceptThread = new Thread("STA Host Server") {
			public void run() {
				while (sta.Running)
					try {
						
						final Client c = new Client(sta.Host.accept());
						c.send("hello");
						sta.Clients.add(c);
						
						Thread clientThread = new Thread("STA Host Client") {
							public void run() {
								while (sta.Running) {
									try { Thread.sleep(50); } catch(InterruptedException ex) {}
									
									String line = c.recv();
									if (line == null) continue;
									
									// Ignore it if it's a duplicate.
									if (sta.lastInsertsAndRemoves.contains(line)) continue;
									
									// Process the line locally (actually type the text).
									String response = sta.processLocal(line, true);
									if (response != null) c.send(response);
									
									// Then forward it to the other clients.
									for (Client cto : sta.Clients) 
										if (c != cto)
											cto.send(line);
								}
							}
						};
						clientThread.setDaemon(true);
						clientThread.start();

					} catch (IOException e) {
						e.printStackTrace();
					}
			}
		};
		serverAcceptThread.setDaemon(true);
		serverAcceptThread.start();
		
		// Add the document listener and start the sync timer.
		sta.code.getDocument().addDocumentListener(new NetworkedDocumentListener(sta));
		sta.SyncTimer = new SyncTimer(sta);
		sta.SyncTimer.start();
		
		return sta;
	}

	/**
	 * Create a new text area connected to a given target.
	 * @param connectTo IP:Port
	 * @throws Exception If we can't get the server.
	 */
	public static SharedTextArea join(String connectTo) throws Exception {
		final SharedTextArea sta = new SharedTextArea();
		sta.ID = connectTo;
		
		// Unconvert bad characters
		for (char[] badPair : BAD_CHARS) 
			connectTo = connectTo.replace(badPair[1], badPair[0]);
		
		// Decode port and IP.
		byte[] data = Base64.decode(connectTo);
		byte[] addr = Arrays.copyOf(data, data.length - 2);
		InetAddress ip = InetAddress.getByAddress(addr);
		int lo = ((int) data[data.length - 2]) + 128;
		int hi = ((int) data[data.length - 1]) + 128;
		int port = lo * 256 + hi;
		
		// Create a socket for it.
		sta.Server = new Client(new Socket(ip, port));
		sta.Server.send("hello");
		
		// Set up a listening thread.
		Thread fromServerThread = new Thread("STA Join") {
			public void run() {
				while (sta.Running) {
					try { Thread.sleep(50); } catch(InterruptedException ex) {}
					
					String line = sta.Server.recv();
					if (line != null) {
						String response = sta.processLocal(line, false);
						if (response != null) sta.Server.send(response);
					}
				}
			}
		};
		fromServerThread.setDaemon(true);
		fromServerThread.start();
		
		// Add the document listener and start the sync timer.
		sta.code.getDocument().addDocumentListener(new NetworkedDocumentListener(sta));
		sta.SyncTimer = new SyncTimer(sta);
//		sta.SyncTimer.start();
		// ^ disabled to force clients to sync to the server rather than the other way around
		
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
	 * @param onServer If we're doing something on the host, versus on a join'er.
	 * @return Any response to be sent back.
	 */
	protected String processLocal(String line, boolean onHost) {
		String[] parts = line.split(",");
		
		try {
			
			if ("hello".equals(parts[0])) {
				
				if (onHost && getText().length() > 0)
					return "force-sync," + Base64.encodeBytes(getText().getBytes("UTF-8"));
				else
					return null;
				
			} if ("insert".equals(parts[0])) {
				
				int off = Integer.parseInt(parts[1]);
				String str = new String(Base64.decode(parts[3]), "UTF-8");
				
				if (lastInsertsAndRemoves.contains(line)) return null;

				try {
					SyncTimer.setActive();
					code.getDocument().insertString(off, str, null);
					return null;
				} catch(BadLocationException e) {
					return "check-sync," + getText().hashCode();
				}
				
			} else if ("remove".equals(parts[0])) {
				
				int off = Integer.parseInt(parts[1]);
				int len = Integer.parseInt(parts[2]);
				
				if (lastInsertsAndRemoves.contains(line)) return null;

				try {
					SyncTimer.setActive();
					code.getDocument().remove(off, len);
					return null;
				} catch(BadLocationException e) {
					return "check-sync," + getText().hashCode();
				}
				
			} else if ("check-sync".equals(parts[0])) {
				
				int hash = Integer.parseInt(parts[1]);
				int myHash = getText().hashCode();
				
				if (hash != myHash)
					return "request-sync";
				else
					return null;
				
			} else if ("request-sync".equals(parts[0])) {
				
				return "force-sync," + Base64.encodeBytes(getText().getBytes("UTF-8"));
				
			} else if ("force-sync".equals(parts[0])) {
				
				SyncTimer.setActive();
				String str = new String(Base64.decode(parts[1]), "UTF-8");
				code.setText(str);
				return null;
				
			}
			
		} catch(Exception ex) {
			ex.printStackTrace();
		}
		
		return null;
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
	public void close() {
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
		if (SharedTextArea.NETWORKING_DEBUG)  // debug
			System.out.println("send to " + Socket.getInetAddress().getHostAddress() + ":" + Socket.getPort() + " -- " + msg);
		
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
			
			if (SharedTextArea.NETWORKING_DEBUG)  // debug
				System.out.println("recv from " + Socket.getInetAddress().getHostAddress() + ":" + Socket.getPort() + " -- " + msg); 
			
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
			
			STA.SyncTimer.setActive();
			
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes("UTF-8"));
			
			String msg = "insert," + off + "," + len + "," + str;
			STA.lastInsertsAndRemoves.add(msg);
			
			if (STA.Server != null) 
				STA.Server.send(msg);
			
			if (STA.Clients != null) 
				for (Client c : STA.Clients)
					c.send(msg);

		} catch(Exception e) {
		}
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
		
		STA.SyncTimer.setActive();
		
		int off = event.getOffset();
		int len = event.getLength();
		
		String msg = "remove," + off + "," + len;
		STA.lastInsertsAndRemoves.add(msg);
		
		if (STA.Server != null) 
			STA.Server.send(msg);
		
		if (STA.Clients != null) 
			for (Client c : STA.Clients)
				c.send(msg);
		
	}
}

/**
 * Timer that keeps things syncronized.
 * @author verkampj
 *
 */
class SyncTimer extends Timer {
	private static final long serialVersionUID = 8137378222084313020L;
	private boolean DocumentActive = false;
	private boolean SyncedLastInactive = false;
	
	/**
	 * Create a sync timer for a given text area.
	 * @param sta
	 */
	public SyncTimer(final SharedTextArea sta) {
		super(1000, null);
		
		addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				
				// Only sync when the previous cycle wasn't active ... 
				if (!DocumentActive) {
					// ... and we didn't already sync.
					if (!SyncedLastInactive) {
						
						String msg = "check-sync," + sta.getText().hashCode();
						
						if (sta.Server != null)
							sta.Server.send(msg);
						
						if (sta.Clients != null)
							for (Client c : sta.Clients)
								c.send(msg);
						
						SyncedLastInactive = true;
						
					} 
				}
				
				// Reset the active flag.
				DocumentActive = false;
			}
		});
		
		setCoalesce(true);
		setDelay(1000);
		setInitialDelay(1000);
	}
	
	/**
	 * The document is active, don't continue.
	 */
	public void setActive() {
		DocumentActive = true;
		SyncedLastInactive = false;
	}
}