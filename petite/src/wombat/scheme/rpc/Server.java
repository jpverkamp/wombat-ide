package wombat.scheme.rpc;

import java.io.*;
import java.net.*;
import java.util.*;

import wombat.scheme.libraries.Conversion;
import wombat.scheme.libraries.ImageAPI;
import wombat.scheme.libraries.ImageData;
import wombat.util.Base64;

/**
 * An RPC server that will listen for specially formatted requests.
 * 
 * Used to implement the Java version of 
 */
public class Server extends Thread {
	int Port;

	/**
	 * Create a new server on the given port.
	 * @param port The port number.
	 */
	public Server(int port) {
		Port = port;
	}
	
	
	/**
	 * Run the server on the given port.
	 * @param port
	 */
	public void run() {
		try {
			
			ServerSocket server = new ServerSocket(Port);
			
			while (!server.isClosed()) {
				final Socket client = server.accept();
				Thread clientThread = new Thread() {
					public void run() {
						Scanner fromClient = null;
						PrintWriter toClient = null;
						
						try {
							fromClient = new Scanner(client.getInputStream());
							toClient = new PrintWriter(client.getOutputStream());
							
							while (true) {
								String[] cmd = fromClient.nextLine().split(" ");

								if ("read-image".equals(cmd[0])) {
									ImageData img;
									if (cmd.length == 1)
										img = ImageAPI.readImage();
									else
										img = ImageAPI.readImage(cmd[1]);
									
									toClient.write(img.Width + " " + img.Height + " ");
									toClient.write(Base64.encodeBytes(Conversion.int2byte(img.Data)));
									toClient.write("\n");
								} else if ("write-image".equals(cmd[0])) {
									int width = Integer.parseInt(cmd[1]);
									int height = Integer.parseInt(cmd[2]);
									
									if (cmd.length == 4) {
										int[] data = Conversion.byte2int(Base64.decode(cmd[3]));
										ImageAPI.writeImage(new ImageData(width, height, data));
									} else {
										String filename = cmd[3];
										int[] data = Conversion.byte2int(Base64.decode(cmd[4]));
										ImageAPI.writeImage(new ImageData(width, height, data), filename);
									}
								} else if ("display-image".equals(cmd[0])) {
									int width = Integer.parseInt(cmd[0]);
									int height = Integer.parseInt(cmd[1]);
									int[] data = Conversion.byte2int(Base64.decode(cmd[4]));
									ImageAPI.displayImage(new ImageData(width, height, data));
								}
								
								toClient.flush();
							}
							
						} catch (IOException e) {
							e.printStackTrace();
						} finally {
							if (fromClient != null) fromClient.close();
							if (toClient != null) toClient.close();
							try { client.close(); } catch (IOException e) {}
						}
					}
				};
				clientThread.setDaemon(true);
				clientThread.start();
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		
	}

}
