package wombat.launcher;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.io.*;
import java.net.*;
import java.util.*;
import java.util.prefs.Preferences;

import javax.swing.*;

/**
 * Update Wombat.
 */
public class Updater extends Thread {
	private Updater() {}
	
	public static final String UPDATE_SITE = "http://www.cs.indiana.edu/cgi-pub/c211/wombat/";
	public static final String VERSION_FILE = "version.txt"; 

	static boolean suppressGUI = false;
	static final JFrame updateFrame = new JFrame("Downloading files...");
	static final JProgressBar currentProgress = new JProgressBar();
	static final JProgressBar overallProgress = new JProgressBar();
	
	static Updater lock = new Updater();
	
	static {
		// Build the frame that will show update progress.
		updateFrame.setSize(400, 200);
		updateFrame.setLocationByPlatform(true);
		updateFrame.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		updateFrame.setLayout(new BorderLayout());
		
		updateFrame.add(getSpacer(20, 20), BorderLayout.WEST);
		updateFrame.add(getSpacer(20, 20), BorderLayout.EAST);
		updateFrame.add(getSpacer(20, 20), BorderLayout.SOUTH);
		
		// Add the status bars.
		JPanel progressPanel = new JPanel();
		progressPanel.setLayout(new GridLayout(4, 1));
		progressPanel.add(new JLabel("Current file:"));
		progressPanel.add(currentProgress);
		progressPanel.add(new JLabel("Overall:"));
		progressPanel.add(overallProgress);
		updateFrame.add(progressPanel, BorderLayout.CENTER);

		currentProgress.setStringPainted(true);
		overallProgress.setStringPainted(true);
	}
	
	/**
	 * Check if Wombat needs to be updated.
	 * @return If it needs an update.
	 */
	public static boolean needsUpdate() throws MalformedURLException, IOException {
		suppressGUI = false;
		
		Map<String, Version> curVersions = Version.parseVersions();
		Map<String, Version> newVersions = Version.parseVersions(download(new URL(UPDATE_SITE + VERSION_FILE)));
		
		for (String name : newVersions.keySet()) {
			if (!curVersions.containsKey(name)) {
				System.out.println("Need to update '" + name + "', it doesn't exist.");
				return true;
			} else if (curVersions.get(name).compareTo(newVersions.get(name)) < 0) {
				System.out.println("Need to update '" + name + "' from version " + curVersions.get(name).Version + " to " + newVersions.get(name).Version);
				return true;
			} else {
				System.out.println("'" + name + "' is up to date. (" + curVersions.get(name).Version + " vs " + newVersions.get(name).Version + ")");
			}
		}
		
		return false;
	}
	
	/**
	 * Update Wombat and then launch.
	 * @param force To force the update.
	 */
	public static void updateAndLaunch(boolean force) throws ClassNotFoundException, InstantiationException, IllegalAccessException, IOException {
		suppressGUI = false;
		
		try {
			update(force);
			synchronized (lock) {
				lock.wait();
			}
		} catch(InterruptedException ex) {
		} catch(IllegalMonitorStateException ex) {
		} finally {
			Launcher.launch();
		}
	}
	
	/**
	 * Update Wombat and display a message when finished.
	 */
	public static void updateAndReport() {
		suppressGUI = true;
		
		try {
			update(false);
			synchronized (lock) {
				lock.wait();
			}
		} catch(InterruptedException ex) {
		} catch(IllegalMonitorStateException ex) {
		} catch (MalformedURLException ex) {
			ex.printStackTrace();
			JOptionPane.showMessageDialog(
					null, 
					"Wombat could not be updated.\nError: " + ex.getMessage(), 
					"Wombat Update",
					JOptionPane.ERROR_MESSAGE);
			return;
			
		} catch (IOException ex) {
			ex.printStackTrace();
			JOptionPane.showMessageDialog(
					null, 
					"Wombat could not be updated.\nError: " + ex.getMessage(), 
					"Wombat Update",
					JOptionPane.ERROR_MESSAGE);
			return;
			
		} finally {
			JOptionPane.showMessageDialog(
					null, 
					"Wombat has been updated.\nThe updates will take place the next time you restart Wombat.", 
					"Wombat Update",
					JOptionPane.INFORMATION_MESSAGE);
		}
	}
	
	/**
	 * Check for and potentially update Wombat.
	 */
	public static void update(boolean force) throws MalformedURLException, IOException {
		String path = ".";
		try {
			path = new File(Version.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile().getAbsolutePath();
		} catch (URISyntaxException e1) {
		}
		
		updateFrame.setVisible(!suppressGUI);
		currentProgress.setIndeterminate(true);
		
		// Load version information.
		final Map<String, Version> curVersions = force ? Version.parseVersions("") : Version.parseVersions();
		final Map<String, Version> newVersions = Version.parseVersions(download(new URL(UPDATE_SITE + VERSION_FILE)));
		
		// Set up the progress bars.
		currentProgress.setString("Downloading version information...");		
		
		overallProgress.setMinimum(0);
		overallProgress.setValue(0);
		overallProgress.setMaximum(newVersions.size());
		overallProgress.setString(overallProgress.getValue() + " / " + overallProgress.getMaximum());

		// Loop through the versions we need.
		for (String name : newVersions.keySet()) {
			// Is it new or not? Continue if not.
			if (!curVersions.containsKey(name)) {
				log("Installing " + name + " at (" + newVersions.get(name).Version + ")");
			} else if (curVersions.get(name).compareTo(newVersions.get(name)) < 0) {
				log("Updating " + name + " from " + curVersions.get(name).Version + " to " + newVersions.get(name).Version);
			} else {
				log(name + " is up to date.");
				continue;
			}
			
			// Download the new file.
			download(
				newVersions.get(name).Name, 
				new URL(UPDATE_SITE + newVersions.get(name).File), 
				new File(path, newVersions.get(name).File)
			);

			
			// Remove the old file.
			if (curVersions.containsKey(name))
				remove(curVersions.get(name).File);
			
			// Add the new file to the index.
			curVersions.put(name, newVersions.get(name));
			
			// Update the overall progress bar.
			overallProgress.setValue(overallProgress.getValue() + 1);
			overallProgress.setString(overallProgress.getValue() + " / " + overallProgress.getMaximum());
		}
		
		// Done updating, so hide the update frame.
		updateFrame.setVisible(false);

		// Get the new version string.
		StringBuilder sb = new StringBuilder();
		for (Version v : curVersions.values()) {
			sb.append(v.Name);
			sb.append(',');
			sb.append(v.Version);
			sb.append(',');
			sb.append(v.File);
			sb.append('\n');
		}
		
		// Dump it to a version file.
		File f = new File(path, "version.txt");
		FileWriter fw = new FileWriter(f);
		fw.write(sb.toString());
		fw.flush();
		fw.close();
		
		// Notify any waiting threads when we finish.
		lock.notifyAll();
	}
	
	/**
	 * Space things out.
	 * @param width Width to add.
	 * @param height Height to add.
	 * @return The spacer.
	 */
	private static JPanel getSpacer(int width, int height) {
		JPanel spacer = new JPanel();
		spacer.setSize(width, height);
		return spacer;
	}

	/**
	 * Remove a local file after it's been updated.
	 * @param filename The file to remove.
	 */
	static void remove(String filename) {
		Preferences prefs = Preferences.userRoot().node("wombat");
		prefs.put("old-versions", prefs.get("old-versions", "") + ";" + filename);
	}

	/**
	 * Download a file into a string.
	 * 
	 * @param from
	 *            The source URL.
	 * @return The contents as a string.
	 */
	static String download(URL from) throws IOException {
		URLConnection connection = from.openConnection();
		int length = connection.getContentLength();
		
		currentProgress.setIndeterminate(false);
		currentProgress.setMinimum(0);
		currentProgress.setValue(0);
		if (length != -1)
			currentProgress.setMaximum(length);
		else
			currentProgress.setIndeterminate(true);
		
		StringBuilder out = new StringBuilder();
		BufferedInputStream in = new BufferedInputStream(from.openStream());

		int count;
		byte data[] = new byte[1024];
		while ((count = in.read(data, 0, 1024)) > 0) {
			out.append(new String(data, 0, count));
			
			if (length != -1)
				currentProgress.setValue(currentProgress.getValue() + count);
			
			System.out.println("repainting");
			updateFrame.repaint();
		}
		if (length != -1)
			currentProgress.setValue(currentProgress.getMaximum());

		in.close();

		return out.toString();
	}

	/**
	 * Download a file to another file.
	 * 
	 * @param from
	 *            The source URL.
	 * @param to
	 *            The destination file.
	 */
	static void download(String name, URL from, File to) throws IOException {
		URLConnection connection = from.openConnection();
		int length = connection.getContentLength();
		
		int overallMax = overallProgress.getMaximum();
		int overallNow = overallProgress.getValue();

		currentProgress.setString(name);
		currentProgress.setIndeterminate(false);
		currentProgress.setMinimum(0);
		currentProgress.setValue(0);
		if (length != -1) {
			currentProgress.setMaximum(length);
			
			overallProgress.setMaximum(length * overallMax);
			overallProgress.setValue(length * overallNow);
		} else {
			currentProgress.setIndeterminate(true);
		}
		
		BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(to));
		BufferedInputStream in = new BufferedInputStream(connection.getInputStream());

		int count;
		byte data[] = new byte[10240];
		while ((count = in.read(data, 0, 10240)) > 0) {
			if (length != -1) {
				currentProgress.setValue(currentProgress.getValue() + count);
				overallProgress.setValue(overallProgress.getValue() + count);
			}
			out.write(data, 0, count);
		}
		if (length != -1) {
			currentProgress.setValue(currentProgress.getMaximum());
			
			overallProgress.setMaximum(overallMax);
			overallProgress.setValue(overallNow);
		}

		out.close();
		in.close();
	}
	
	/**
	 * Log messages.
	 * TODO: replace this.
	 * 
	 * @param msg The message to log.
	 */
	static void log(String msg) {
		System.out.println(msg);
	}
}
