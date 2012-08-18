/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;

import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import wombat.util.Options;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	/**
	 * Create a new text area.
	 */
	private SharedTextArea(boolean marginLine, boolean lineNumbers) {
		super(marginLine, lineNumbers);
		
		// Custom text pane that displays server information.
		code = new LinedTextPane(this) {
			private static final long serialVersionUID = 7284878426815837099L;

			@Override public void paint(Graphics go) {
		    	super.paint(go);
		    	
		    	Graphics2D g = (Graphics2D) go;
		    	int width = 2 + 80 * g.getFontMetrics(new Font("Monospaced", Font.PLAIN, Options.FontSize)).charWidth(' '); 
		    	g.setColor(Color.LIGHT_GRAY);
		    	
		    	g.drawString("Hello world!", width + 10, 18);
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
		final SharedTextArea sta = new SharedTextArea(true, true);
		
		return sta;
	}

	/**
	 * Create a new text area connected to a given target.
	 * @param connectTo IP:Port
	 * @throws Exception If we can't get the server.
	 */
	public static SharedTextArea join(String connectTo) throws Exception {
		final SharedTextArea sta = new SharedTextArea(true, true);
		
		return sta;
	}
	
	/**
	 * Process a line locally, either as a host or when joined.
	 * @param line The new line input.
	 * @param onServer If we're doing something on the host, versus on a join'er.
	 * @return Any response to be sent back.
	 */
	protected String processLocal(String line, boolean onHost) {
//		String[] parts = line.split(",");
//		
//		try {
//			
//			// Initial login, used by the server to send the initial state to new clients.
//			if ("hello".equals(parts[0])) {
//				
//				if (onHost && getText().length() > 0)
//					return "force-sync," + Base64.encodeBytes(getText().getBytes("UTF-8"));
//				else
//					return null;
//				
//			} 
//			
//			// Text has been inserted into the remote document.
//			else if ("insert".equals(parts[0])) {
//				
//				int off = Integer.parseInt(parts[1]);
//				String str = new String(Base64.decode(parts[3]), "UTF-8");
//				
//				if (lastInsertsAndRemoves.contains(line)) return null;
//
//				try {
//					SyncTimer.setActive();
//					code.getDocument().insertString(off, str, null);
//					return null;
//				} catch(BadLocationException e) {
//					return "check-sync," + getText().hashCode();
//				}
//				
//			}
//			
//			// Text has been removed from the remote document.
//			else if ("remove".equals(parts[0])) {
//				
//				int off = Integer.parseInt(parts[1]);
//				int len = Integer.parseInt(parts[2]);
//				
//				if (lastInsertsAndRemoves.contains(line)) return null;
//
//				try {
//					SyncTimer.setActive();
//					code.getDocument().remove(off, len);
//					return null;
//				} catch(BadLocationException e) {
//					return "check-sync," + getText().hashCode();
//				}
//				
//			}
//			
//			// Remote document wants to check that both are in sync.
//			else if ("check-sync".equals(parts[0])) {
//				
//				int hash = Integer.parseInt(parts[1]);
//				int myHash = getText().hashCode();
//				
//				if (hash != myHash)
//					return "request-sync";
//				else
//					return null;
//				
//			} 
//			
//			// The remote document thinks it's out of sync and wants to get back into sync.
//			else if ("request-sync".equals(parts[0])) {
//				
//				return "force-sync," + Base64.encodeBytes(getText().getBytes("UTF-8"));
//				
//			}
//			
//			// The remote document has sent a new version to override our version.
//			else if ("force-sync".equals(parts[0])) {
//				
//				SyncTimer.setActive();
//				String str = new String(Base64.decode(parts[1]), "UTF-8");
//				code.setText(str);
//				return null;
//				
//			}
//			
//		} catch(Exception ex) {
//			ex.printStackTrace();
//		}
		
		return null;
	}

	/**
	 * ID accessor.
	 * @return The ID.
	 */
	public String getID() {
//		return ID;
		return null;
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
//		if (SharedTextArea.NETWORKING_DEBUG)  // debug
//			System.out.println("send to " + Socket.getInetAddress().getHostAddress() + ":" + Socket.getPort() + " -- " + msg);
//		
//		To.println(msg); 
//		To.flush();
	}
	
	/**
	 * Next line from the client or null.
	 * @return Read
	 */
	public String recv() {
//		if (From.hasNextLine()) {
//			String msg = From.nextLine();
//			
//			if (SharedTextArea.NETWORKING_DEBUG)  // debug
//				System.out.println("recv from " + Socket.getInetAddress().getHostAddress() + ":" + Socket.getPort() + " -- " + msg); 
//			
//			return msg;
//		} else {
			return null;
//		}
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
//		try {
//			
//			STA.SyncTimer.setActive();
//			
//			int off = event.getOffset();
//			int len = event.getLength();
//			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes("UTF-8"));
//			
//			String msg = "insert," + off + "," + len + "," + str;
//			STA.lastInsertsAndRemoves.add(msg);
//			
//			if (STA.Server != null) 
//				STA.Server.send(msg);
//			
//			if (STA.Clients != null) 
//				for (Client c : STA.Clients)
//					c.send(msg);
//
//		} catch(Exception e) {
//		}
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
//		
//		STA.SyncTimer.setActive();
//		
//		int off = event.getOffset();
//		int len = event.getLength();
//		
//		String msg = "remove," + off + "," + len;
//		STA.lastInsertsAndRemoves.add(msg);
//		
//		if (STA.Server != null) 
//			STA.Server.send(msg);
//		
//		if (STA.Clients != null) 
//			for (Client c : STA.Clients)
//				c.send(msg);
//		
	}
}