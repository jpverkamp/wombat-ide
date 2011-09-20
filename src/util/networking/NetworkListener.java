package util.networking;

import java.net.Socket;

public abstract class NetworkListener {
	public abstract void onReceive(String content); 
	
	public abstract void onConnection(Socket client);
}
