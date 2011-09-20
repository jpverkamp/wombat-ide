package util.networking;

import java.io.*;
import java.net.*;
import java.util.*;

import util.errors.ErrorManager;

public class NetworkManager {
	final static int Port = 6992;
	
	static List<ClientThread> clients = new ArrayList<ClientThread>();
	static List<NetworkListener> listeners = new ArrayList<NetworkListener>();
	static boolean NetworkRunning;
	
	private NetworkManager() {}
	
	public static void addNetworkListener(NetworkListener nl) {
		listeners.add(nl);
	}
	
	public static void removeNetworkListener(NetworkListener nl) {
		listeners.remove(nl);
	}
	
	public static void host() {
		if (NetworkRunning) return;
		
		new ServerThread();
		NetworkRunning = true;
	}
	
	public static void join(String targetIP) {
		if (NetworkRunning) return;
		
		try {
			Socket server = new Socket(InetAddress.getByName(targetIP), Port);
			clients.add(new ClientThread(server));
			NetworkRunning = true;
			
		} catch (UnknownHostException e) {
			ErrorManager.logError("Error in join: " + e.getMessage());
		} catch (IOException e) {
			ErrorManager.logError("Error in join: " + e.getMessage());
		}
	}
	
	public static void broadcast(String message) {
		for (ClientThread client : clients)
			client.send(message);
	}
	
	public static void disconnect() {
		NetworkRunning = false;
	}
}

class ServerThread implements Runnable {

	public ServerThread() {
		Thread t = new Thread(this);
		t.setDaemon(true);
		t.start();
	}
	
	@Override
	public void run() {
		try {
			ServerSocket server = new ServerSocket(NetworkManager.Port);
			NetworkManager.NetworkRunning = true;
			
			while (NetworkManager.NetworkRunning) {
				Socket client = server.accept();
				
				for (NetworkListener nl : NetworkManager.listeners)
					nl.onConnection(client);
				
				new ClientThread(client);
			}
			
			server.close();
			NetworkManager.clients.clear();
			
		} catch (IOException e) {
			ErrorManager.logError("Error in ServerThread: " + e.getMessage());
		}
	}
	
}

class ClientThread implements Runnable {
	Socket Client;
	Queue<String> toSend = new LinkedList<String>();
	
	public ClientThread(Socket client) {
		Client = client;
		
		Thread t = new Thread(this);
		t.setDaemon(true);
		t.start();
	}
	
	public void send(String message) {
		toSend.add(message);
	}
	
	@Override
	public void run() {
		try {
			Scanner fromClient = new Scanner(Client.getInputStream());
			PrintWriter toClient = new PrintWriter(Client.getOutputStream());
			
			while (NetworkManager.NetworkRunning) {
				if (!toSend.isEmpty())
					toClient.println(toSend.remove());
				
				if (fromClient.hasNextLine()) {
					String recv = fromClient.nextLine();
					for (NetworkListener nl : NetworkManager.listeners)
						nl.onReceive(recv);
				}
				
				try { Thread.sleep(50); } catch(InterruptedException ie) {}
			}
			
			fromClient.close();
			toClient.close();
			Client.close();
			
		} catch (IOException e) {
			ErrorManager.logError("Error in ClientThread: " + e.getMessage()); 
		}
		
	}
}