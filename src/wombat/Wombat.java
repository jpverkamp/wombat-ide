package wombat;

import gui.MainFrame;

/**
 * Main entry point of the program.
 */
public class Wombat {
	public static final int VERSION = 165;
	
    public static void main(String[] args) {
    	System.setSecurityManager(null);
    	new Wombat();
    }

    private Wombat() {
    	MainFrame main = new MainFrame();
        main.setVisible(true);
    }
}
