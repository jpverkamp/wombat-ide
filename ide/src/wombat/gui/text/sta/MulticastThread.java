package wombat.gui.text.sta;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;

public abstract class MulticastThread extends Thread{
	public static final String MULTICAST_IP = "224.5.30.9";
	public static final int MULTICAST_PORT = 5309;
	
	MulticastSocket MS;
	
	/**
	 * Create a named thread.
	 * @param name The name.
	 * @throws IOException If we cannot talk with the multicast socket.
	 * @throws UnknownHostException If we can't figure out who to talk to.
	 */
	public MulticastThread(String name) throws UnknownHostException, IOException {
		super(name);
		
		MS = new MulticastSocket(MULTICAST_PORT);
		MS.joinGroup(InetAddress.getByName(MULTICAST_IP));

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
		return new String(packet.getData());
	}
	
	/**
	 * Send out a multicast packet. 
	 * @param msg The message to send.
	 * @throws IOException If we cannot send the message.
	 */
	void multicastSend(String msg) throws IOException {
		DatagramPacket packet = new DatagramPacket(msg.getBytes(), msg.getBytes().length);
		MS.send(packet);
	}

}
