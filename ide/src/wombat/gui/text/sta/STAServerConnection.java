package wombat.gui.text.sta;

import java.io.PrintWriter;
import java.net.Socket;
import java.util.Scanner;

/**
 * Thread that runs on the server to keep track of any connected clients.
 */
public class STAServerConnection extends Thread {
	STAServer ST;
	Socket C;
	
	PrintWriter ToClient;
	Scanner FromClient;
	
	/**
	 * Create a new server client thread.
	 * @param st
	 */
	public STAServerConnection(STAServer st, Socket c) {
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
			for (STAServerConnection ea : ST.Clients) {
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
					for (STAServerConnection ea : ST.Clients) {
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