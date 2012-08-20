/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text;

import java.awt.Color;

import java.io.*;
import java.net.*;
import java.util.Arrays;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.BadLocationException;

import wombat.util.Base64;
import wombat.util.NameGenerator;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	// enable this to print all packets
	public static final boolean NETWORK_DEBUG = false;
	
	String DocumentName;
	String MyName;
	
	MulticastThread MT;
	NetworkedDocumentListener NDL;
	
	public SharedTextArea(String name) {
		super(true, true);
		DocumentName = name;
		MyName = NameGenerator.getName();
		
		// Custom text pane that displays server information.
		code = new LinedTextPane(this);
        add(new JScrollPane(code));
        
        // Distinguish it from other text areas.
        code.setBackground(new Color(240, 255, 240));
        
        // Attach the document listener.
        NDL = new NetworkedDocumentListener(this);
        code.getDocument().addDocumentListener(NDL);
        
        // Now we need to establish the connections.
        try {
        	MT = new MulticastThread(this);
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
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
			String lineDocName = parts[0];
			String lineSrcName = parts[1];
			String lineMsgType = parts[2];
			String[] args = Arrays.copyOfRange(parts, 3, parts.length); 
			
			// Ignore ones that are to the wrong multicast network.
			if (!lineDocName.equals(DocumentName)) {
			}
			
			// Ignore messages that came from me.
			else if (lineSrcName.equals(MyName)) {
			}
			
			// Text has been inserted into the remote document.
			else if ("insert".equals(lineMsgType)) {
				
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
		msg.append(DocumentName);
		msg.append(',');
		msg.append(MyName);
		msg.append(',');
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
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes("UTF-8"));
			
			STA.MT.multicastSend(STA.makeMessage("insert", off, len, str));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			
			STA.MT.multicastSend(STA.makeMessage("remove", off, len));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
}

/**
 * Controls the multicast network.
 */
class MulticastThread extends Thread {
	public static InetAddress MULTICAST_GROUP;
	public static int MULTICAST_PORT = 5309;
	
	static {
		try {
			MULTICAST_GROUP = InetAddress.getByName("224.5.30.9");
		} catch(Exception e) {
		}
	}
	
	public boolean Running = true;
	
	SharedTextArea STA;
	MulticastSocket MS;
	
	/**
	 * Create a named thread.
	 * @param sta 
	 * @param name The name.
	 * @throws IOException If we cannot talk with the multicast socket.
	 * @throws UnknownHostException If we can't figure out who to talk to.
	 */
	public MulticastThread(SharedTextArea sta) throws UnknownHostException, IOException {
		super("STA multicast thread");
		
		STA = sta;
		
		MS = new MulticastSocket(MULTICAST_PORT);
		MS.setTimeToLive(4);
		MS.joinGroup(MULTICAST_GROUP);
		
		setDaemon(true);
		start();
	}
	
	/**
	 * Keep receiving messages.
	 */
	public void run() {
		while (Running) {
			try {
				
				String msg = multicastReceive();
				STA.processLocal(msg);
				
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Receive the next datagram from the given multicast socket.
	 * @return A string representing the datagram.
	 * @throws IOException If we cannot read from the multicast.
	 */
	String multicastReceive() throws IOException {
		byte[] buffer = new byte[1024];
		
		DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
		MS.receive(packet);
		String msg = new String(packet.getData(), packet.getOffset(), packet.getLength());
		
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("mult recv: " + msg);
		
		return msg;
	}
	
	/**
	 * Send out a multicast packet. 
	 * @param msg The message to send.
	 * @throws IOException If we cannot send the message.
	 */
	void multicastSend(String msg) throws IOException {
		if (SharedTextArea.NETWORK_DEBUG)
			System.out.println("mult send: " + msg);
		
		DatagramPacket packet = new DatagramPacket(msg.getBytes(), msg.getBytes().length, MULTICAST_GROUP, MULTICAST_PORT);
		MS.send(packet);
	}

}
