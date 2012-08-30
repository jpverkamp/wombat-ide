/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text.sta;

import java.awt.Color;
import java.io.*;
import java.net.*;
import java.util.*;

import javax.swing.*;
import javax.swing.text.BadLocationException;

import wombat.util.Base64;

import wombat.gui.text.LinedTextPane;
import wombat.gui.text.SchemeTextArea;

/**
 * Designed to share a text area between two or more users over a local network. 
 */
public class SharedTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 2220038488909999007L;
	
	public static final boolean NETWORK_DEBUG = false;
	public static int NEXT_PORT = 5309;
	
	boolean Running = true;
	String DocumentName;
	STAServer ST;
	STAClient CT;
	
	NetworkedDocumentListener NDL;
	
	/**
	 * Create the shared text area.
	 * @param host The address of the server
	 * @param port The port to connect on
	 * @param server We should host the server
	 */
	public SharedTextArea(InetAddress host, int port, boolean server) {
		super(true, true);
		DocumentName = encodeAddress(host, port);
		
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
            	ST = new STAServer(this, port);
            	CT = new STAClient(this, InetAddress.getLocalHost(), port);
            } else {
            	CT = new STAClient(this, host, port);
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
		NDL.suppress(true);
		
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
		
		NDL.suppress(false);
	}
	
	/**
	 * Encode an IP:port address as a Base64 string.
	 * @param host The host to encode
	 * @param port The port to encode
	 * @return The string.
	 */
	public static String encodeAddress(InetAddress host, int port) {
		return Base64.encodeBytes(new byte[]{
				host.getAddress()[0],
				host.getAddress()[1],
				host.getAddress()[2],
				host.getAddress()[3],
				(byte) ((port / 256) - 128),
				(byte) ((port % 256) - 128)
		});
	}
	
	/**
	 * Get the IP out of an encoded string.
	 * @param str The encoded string.
	 * @return The IP
	 */
	public static InetAddress decodeAddressHost(String str) {
		try {
			return Inet4Address.getByAddress(Arrays.copyOf(Base64.decode(str), 4));
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * Get the port out of an encoded string.
	 * @param str The encoded string.
	 * @return The port
	 */
	public static int decodeAddressPort(String str) {
		try {
			byte[] bytes = Base64.decode(str);
			return ((((int) bytes[4]) + 128) * 256) + (((int) bytes[5]) + 128);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return -1;
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
	
	/**
	 * 
	 */
	public boolean isShared() {
		return Running && CT != null;
	}
	
	/**
	 * 
	 */
	public void disconnect() {
		Running = false;
		DocumentName = null;
		ST = null;
		CT = null;
		code.setBackground(Color.WHITE);
	}
}