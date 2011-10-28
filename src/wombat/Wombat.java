package wombat;

import java.io.IOException;
import java.net.MalformedURLException;

import util.errors.ErrorManager;
import wombat.launcher.Updater;
import gui.MainFrame;

/**
 * Main entry point of the program.
 */
public class Wombat {
	public static final String VERSION = "{VERSION}";
	static MainFrame main;
	
	public static void main(String[] argv) {
		System.setSecurityManager(null);
		
		new Wombat();
		
		boolean update = false;
		
		for (String s : argv)
			if ("-update".equals(s.toLowerCase()))
				update = true;
				
		if (update) {
			Thread t = new Thread(new Runnable() {
				public void run() {
					try { Thread.sleep(1000); } catch(InterruptedException ex) {}
					
					try {
						if (Updater.update())
							main.updated();						
					} catch (MalformedURLException ex) {
						ErrorManager.logError("Unable to update Wombat: " + ex.getMessage());
					} catch (IOException ex) {
						ErrorManager.logError("Unable to update Wombat: " + ex.getMessage());
					}
				}
			});
			t.setDaemon(true);
			t.start();
		}
	}

	public Wombat() {
		main = new MainFrame();
		main.setVisible(true);
	}
}
