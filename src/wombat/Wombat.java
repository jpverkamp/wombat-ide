package wombat;

import gui.MainFrame;

import javax.swing.*;

/**
 * Main entry point of the program.
 */
public class Wombat {
	public static final int VERSION = 108;
	
    public static void main(String[] args) {
        new Wombat();
    }

    private Wombat() {
    	JFrame main = MainFrame.me();
        main.setVisible(true);
    }
}
