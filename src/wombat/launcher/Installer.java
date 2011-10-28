package wombat.launcher;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.prefs.Preferences;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;

public class Installer {
	private Installer() {}
	
	/**
	 * Reinstall Wombat.
	 * @throws IOException 
	 * @throws MalformedURLException 
	 */
	public static boolean reinstall() throws MalformedURLException, IOException {
		Preferences prefs = Preferences.userRoot().node("wombat");
		
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
				null,
				"Your version of Wombat is not correctly installed.\nDo you wish to reinstall Wombat?\n\nNOTE: You will not be able to launch Wombat without reinstalling.",
				"Reinstall Wombat?",
				JOptionPane.YES_NO_OPTION)) {
			
			if (get_directory()) {
				if (Updater.update(true)) {
					prefs.putBoolean("installed", true);
					return true;
				} else {
					return false;
				}
			}
		}
		
		return false;
	}
	
	/**
	 * Install Wombat for the first time.
	 * @throws IOException 
	 * @throws MalformedURLException 
	 */
	public static boolean install() throws MalformedURLException, IOException {
		Preferences prefs = Preferences.userRoot().node("wombat");
		
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
				null,
				"Do you wish to install Wombat?\n\nNOTE: You will not be able to launch Wombat without installing.",
				"Install Wombat?",
				JOptionPane.YES_NO_OPTION)) {
			
			if (get_directory()) {
				if (Updater.update(true)) {
					prefs.putBoolean("installed", true);
					return true;
				} else {
					return false;
				}
			}
		}
		
		return false;
	}
	
	/**
	 * Get the installation directory.
	 */
	static boolean get_directory() {
		// Keep trying to get a directory.
		while (true) {
			JFileChooser chooser = new JFileChooser();
			chooser.setCurrentDirectory(new java.io.File("."));
			chooser.setDialogTitle("Install Wombat: Choose a directory");
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
			chooser.setAcceptAllFileFilterUsed(false);

			// They choose a directory.
			if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
				// Check if it's valid.
				File install_dir = chooser.getSelectedFile();
				
				// Invalid directory, ask if they want to try again.
				if (!install_dir.exists() || !install_dir.isDirectory()) {
					if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
							null,
							"You did not choose a valid installation directory.\nDo you still wish to install Wombat?\n\n" +
									"NOTE: You will not be able to launch Wombat without installing.",
							"Install Wombat?",
							JOptionPane.YES_NO_OPTION))
						continue;
					else
						return false;
				}
				
				// Store the installation directory.
				Preferences prefs = Preferences.userRoot().node("wombat");
				prefs.put("install-directory", install_dir.getAbsolutePath());
				return true;
			} 
			
			// They didn't choose a directory, ask if they want to try again.
			else {
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
						null,
						"You did not choose an installation directory.\nDo you still wish to install Wombat?\n\n" +
								"NOTE: You will not be able to launch Wombat without installing.",
						"Install Wombat?",
						JOptionPane.YES_NO_OPTION))
					continue;
				else
					return false;
			}
		}
	}
}
