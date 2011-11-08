package wombat.launcher;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.prefs.Preferences;

import javax.swing.JOptionPane;

/**
 * Launcher for Wombat, also deletes old versions.
 */
public class Launcher {
	/**
	 * Run from the command line.
	 * 
	 * @param args Ignored.
	 */
	public static void main(String[] args) {
		// Delete old versions of files.
		Thread deleteThread = new Thread(new Runnable() {
			public void run() {
				Preferences prefs = Preferences.userRoot().node("wombat");
				List<String> failed = new ArrayList<String>();
				for (String filename : prefs.get("old-versions", "").split(";")) {
					File f = new File(filename);
					if (f.exists() && !f.delete())
						failed.add(filename);
				}
				
				StringBuilder sb = new StringBuilder();
				for (String filename : failed)
					sb.append(";" + filename);
				prefs.put("old-versions", sb.toString());
			}
		}); 
		deleteThread.setDaemon(true);
		deleteThread.start();
		
		// Launch the program.
		try {
			launch();
		} catch (Exception ex) {
			JOptionPane.showMessageDialog(null, "Unable to launch Wombat.\nPlease try again in a few minutes.\n\n" + ex.toString(), "Error", JOptionPane.ERROR_MESSAGE);
			ex.printStackTrace();
			System.exit(1);
		}
	}
	
	public static void launch() throws ClassNotFoundException, InstantiationException, IllegalAccessException, IOException {
		String path = ".";
		try {
			path = new File(Version.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile().getAbsolutePath();
		} catch (URISyntaxException e1) {
		}
		
		// Get local version information (will return an empty map if the installation is missing or corrupted).
		Map<String, Version> current_versions = Version.parseVersions();
		
		// Install if it hasn't been.
		if (current_versions.size() == 0) {
			log("No files installed, need to install.");
			if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
					null,
					"Do you wish to install Wombat?\n\nNOTE: You will not be able to launch Wombat without installing.",
					"Install Wombat?",
					JOptionPane.YES_NO_OPTION)) {
				Updater.updateAndLaunch(true);
				return;
			}
		}

		// Verify that all of the necessary files are present.
		for (Version v : current_versions.values()) {
			if (!new File(path, v.File).exists()) {
				log("Missing file, need to reinstall: " + v.File);
				if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
						null,
						"Your version of Wombat is not correctly installed.\nDo you wish to reinstall Wombat?\n\nNOTE: You will not be able to launch Wombat without reinstalling.",
						"Reinstall Wombat?",
						JOptionPane.YES_NO_OPTION)) {
					Updater.updateAndLaunch(true);
					return;
				}
			}
		}
		
		// Should have installed. Update the versions.
		current_versions = Version.parseVersions(); 
		
		// Get all of the JARs that we might need.
		List<URL> urls = new ArrayList<URL>();
		urls.add(new File(path).toURI().toURL());
		for (Version v : current_versions.values())
			urls.add(new File(path, v.File).toURI().toURL());

		// Build a new class loader.
		ClassLoader currentThreadClassLoader = Thread.currentThread().getContextClassLoader();
		URLClassLoader cl = new URLClassLoader(urls.toArray(new URL[] {}), currentThreadClassLoader);
		Thread.currentThread().setContextClassLoader(cl);

		// Launch Wombat.
		Class<?> cls = Class.forName("wombat.Wombat", true, cl);
		try { cls.getField("AllowUpdate").setBoolean(null, true); } catch (Exception e) {}
		cls.newInstance();
	}

	/**
	 * Log any messages that might come up.
	 */
	private static void log(String msg) {
		System.out.println(msg);
	}
}
