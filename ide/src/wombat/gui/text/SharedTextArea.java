package wombat.gui.text;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
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

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	static final boolean NETWORKING_DEBUG = false;
	
	String ID;
	boolean Running = true;
	
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
		sta.ID = InetAddress.getLocalHost().getHostAddress() + ":" + sta.Host.getLocalPort();
		
		// Start a thread to get new clients.
		Thread serverAcceptThread = new Thread() {
			public void run() {
				while (sta.Running)
					try {
						
						final Client c = new Client(sta.Host.accept());
						c.send("hello");
						sta.Clients.add(c);
						
						Thread clientThread = new Thread() {
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
		
		// Add the document listener.
		final NetworkedDocumentListener NDL = new NetworkedDocumentListener(sta);
		sta.code.getDocument().addDocumentListener(NDL);
		
		// Periodically check for sync.
		Timer t = new Timer(1000, new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				if (NDL.UnsyncedChanges && sta.Clients.size() > 0) {
					String msg = "check-sync," + sta.getText().hashCode();
					for (Client c : sta.Clients)
						c.send(msg);
					NDL.UnsyncedChanges = false;
				}
			}
		});
		t.setCoalesce(true);
		t.setDelay(1000);
		t.start();
		
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
		
		// Add the document listener.
		final NetworkedDocumentListener NDL = new NetworkedDocumentListener(sta);
		sta.code.getDocument().addDocumentListener(NDL);
		
		// Periodically check for sync.
		Timer t = new Timer(1000, new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				if (NDL.UnsyncedChanges) {
					String msg = "check-sync," + sta.getText().hashCode();
					sta.Server.send(msg);
					NDL.UnsyncedChanges = false;
				}
			}
		});
		t.setCoalesce(true);
		t.setDelay(1000);
		t.start();
		
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
					return "force-sync," + Base64.encodeBytes(getText().getBytes());
				else
					return null;
				
			} if ("insert".equals(parts[0])) {
				
				int off = Integer.parseInt(parts[1]);
				String str = new String(Base64.decode(parts[3]));
				
				if (lastInsertsAndRemoves.contains(line)) return null;

				try {
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
					code.getDocument().remove(off, len);
					return null;
				} catch(BadLocationException e) {
					return "check-sync," + getText().hashCode();
				}
				
			} else if ("check-sync".equals(parts[0])) {
				
				int hash = Integer.parseInt(parts[1]);
				int myHash = getText().hashCode();
				
				if (hash != myHash) {
					setBackground(new Color(255, 240, 240));
					
					class WhoDialog extends JDialog {
						private static final long serialVersionUID = 5219519601249391695L;
						
						boolean KeepMine = false;
						
						public WhoDialog() {
							setModal(true);
							setTitle("Out of sync...");
							setLayout(new GridBagLayout());
							
							for (Component c : getComponents()) {
								if (c instanceof AbstractButton)
									c.getParent().remove(c);
							}
							
							GridBagConstraints gbc = new GridBagConstraints();
							gbc.insets = new Insets(5, 5, 5, 5);
							
							gbc.gridx = 0;
							gbc.gridy = 0;
							gbc.gridwidth = 2;
							gbc.gridheight = 1;
							gbc.fill = GridBagConstraints.BOTH;
							add(new JLabel(
									"<html>" +
									"Your documents are out of sync, choose which version to take." +
									"<br />" +
									"Note: If different documents are chosen, this same warning will appear again." + 
									"</html>"
								), gbc);
							
							gbc.gridy = 1;
							gbc.gridwidth = 1;
							gbc.fill = GridBagConstraints.NONE;
							JButton chooseMine = new JButton("Keep mine");
							chooseMine.addActionListener(new ActionListener() {
								@Override public void actionPerformed(ActionEvent arg0) {
									KeepMine = true;
									setVisible(false);
								}
							});
							add(chooseMine, gbc);
							
							gbc.gridx = 1;
							JButton chooseTheirs = new JButton("Keep theirs");
							chooseTheirs.addActionListener(new ActionListener() {
								@Override public void actionPerformed(ActionEvent e) {
									KeepMine = false;  
									setVisible(false);
								}
							});
							add(chooseTheirs, gbc);
							
							pack();
						}
					};
					
					// Set it up and wait for a response.
					WhoDialog checkWho = new WhoDialog();
					checkWho.setVisible(true);
					
					// If we get this far, the user wants to sync and not keep their own.
					if (!checkWho.KeepMine) 
						return "request-sync";
				}
				return null;
				
			} else if ("request-sync".equals(parts[0])) {
				
				return "force-sync," + Base64.encodeBytes(getText().getBytes());
				
			} else if ("force-sync".equals(parts[0])) {
				
				String str = new String(Base64.decode(parts[1]));
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
	boolean UnsyncedChanges = false;

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
			
			UnsyncedChanges = true;
			
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes());
			
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
		
		UnsyncedChanges = true;
		
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