/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat;

import wombat.gui.frames.MainFrame;

/**
 * Main entry point of the program.
 */
public class Wombat {
	// Version number of the program. Automatically filled in with the build script.
	public static final String VERSION = "{VERSION}";

	/**
	 * Run from the command line.
	 * @param argv Command line parameters (ignored)
	 */
	public static void main(String[] argv) {
		new Wombat();
	}

	/**
	 * Actually set up the GUI.
	 */
	public Wombat() {
		MainFrame main = MainFrame.Singleton();
		main.setVisible(true);
	}
}
