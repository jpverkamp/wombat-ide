package wombat.launcher;

import java.io.*;
import java.net.*;
import java.util.prefs.*;
import java.util.*;

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
		try {
			launch();
		} catch (Exception ex) {
			JOptionPane.showMessageDialog(null, "Unable to launch Wombat.\nPlease try again in a few minutes.\n\n" + ex.toString(), "Error", JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}
	}
	
	static void launch() throws ClassNotFoundException, InstantiationException, IllegalAccessException, IOException {
		// Build the preference manager.
		Preferences prefs = Preferences.userRoot().node("wombat");
		
		// Verify that Wombat is installed.
		if (!prefs.getBoolean("installed", false) || prefs.get("install-directory", null) == null) {
			if (Installer.install()) {
				launch();
				return;
			}
		}
		
		// Check that the installation directory exists.
		File dir = new File(prefs.get("install-directory", null));
		if (!dir.exists() || !dir.isDirectory()) {
			log("Installation directory does not exist, need to reinstall.");
			if (Installer.reinstall()) {
				launch();
				return;
			}
		}
		
		// Verify that all of the necessary files are present.
		Map<String, Version> current_versions = Version.parseVersions(prefs.get("versions", ""));
		for (Version v : current_versions.values()) {
			if (!new File(dir, v.File).exists()) {
				log("Missing file, need to reinstall: " + v.File);
				if (Installer.reinstall()) {
					launch();
					return;
				}
			}
		}

		// If we made it this far, then the install worked. 
		log("Running. Install directory: " + prefs.get("install-directory", null));
		
		// Get all of the JARs that we might need.
		List<URL> urls = new ArrayList<URL>();
		urls.add(new File(prefs.get("install-directory", "")).toURI().toURL());
		for (Version v : Version.parseVersions(prefs.get("versions", "")).values())
			urls.add(new File(prefs.get("install-directory", ""), v.File).toURI().toURL());

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
