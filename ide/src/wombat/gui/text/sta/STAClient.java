package wombat.gui.text.sta;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;
import java.util.Scanner;

import wombat.util.NameGenerator;
import wombat.util.errors.ErrorManager;

/**
 * Client thread. Sends local changes and receives remote changes. 
 */
public class STAClient extends Thread {
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
	public STAClient(SharedTextArea sta, InetAddress host, int port) {
		STA = sta;
		MyName = NameGenerator.getName();
		
		try {
			S = new Socket(host, port);
			ToServer = new PrintWriter(S.getOutputStream());
			FromServer = new Scanner(S.getInputStream());
		} catch (IOException e) {
			ErrorManager.logError("Unable to connect to client: " + e);
			STA.code.setText("Unable to connect to client: " + e);
			e.printStackTrace();
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