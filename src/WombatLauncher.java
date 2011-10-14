import java.awt.BorderLayout;
import java.awt.event.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.*;
import java.util.*;
import java.util.jar.Attributes;
import java.util.prefs.*;

import javax.swing.*;

/**
 * Wrapper function used to launch Wombat.
 */
public class WombatLauncher extends JFrame {
	private static final long serialVersionUID = -5352650671126526301L;

	// Update parameters (allow the user to choose these?)
	public static final String remoteDir = "http://www.cs.indiana.edu/cgi-pub/c211/wombat/";
	public static final String versionFile = "version.txt";

	// Gui.
	JTextArea progress = new JTextArea();
	Preferences prefs;

	/**
	 * Run from the command line.
	 * @param args Ignored.
	 */
	public static void main(String[] args) {
		new WombatLauncher();
	}

	/**
	 * Actually do the launch.
	 */
	WombatLauncher() {
		// Main.
		final WombatLauncher me = this;
		
		// Build the preference manager.
		prefs = Preferences.userRoot().node("wombat-launcher");
		
		// Set up the frame.
		setTitle("Wombat Installer");
		setSize(400, 200);
		setLocationByPlatform(true);
		setLayout(new BorderLayout());
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		progress.setEditable(false);
		add(progress, BorderLayout.CENTER);
		
		JPanel buttonPanel = new JPanel();
		add(buttonPanel, BorderLayout.SOUTH);
		
		// A button to launch Wombat. This will be disabled until the install/update goes through.
		final JButton launchButton = new JButton("Launch Wombat");
		launchButton.setEnabled(false);
		launchButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				
			}
		});
		buttonPanel.add(launchButton, BorderLayout.WEST);

		// Run the install / update in another thread to keep the GUI responsive.
		Thread t = new Thread(new Runnable() {
			@Override
			public void run() {
				// Try to update / install the code.
				try {
					if (!prefs.getBoolean("installed", false))
						install();
					else
						update();
				} catch (MalformedURLException e) {
					e.printStackTrace();
					JOptionPane.showMessageDialog(me, "Error:\n" + e, "Unable to launch Wombat", JOptionPane.ERROR_MESSAGE);
				} catch (IOException e) {
					e.printStackTrace();
					JOptionPane.showMessageDialog(me, "Error:\n" + e, "Unable to launch Wombat", JOptionPane.ERROR_MESSAGE);
				}

				// If we get this far and we never show'ed the dialog, just run directly.
				if (isVisible())
					launchButton.setEnabled(true);
				else
					launch();
			}
		});
		t.setDaemon(true);
		t.start();
	}
	
	/**
	 * Launch Wombat.
	 */
	void launch() {
		try {
			log("");
			log("Launching Wombat...");
			
			JarClassLoader cl = new JarClassLoader(new URL("file://" + prefs.get("install-directory", "")));
			
			for (Version v : parseVersions(prefs.get("versions", "")).values())
				cl.addFile(new File(prefs.get("install-directory", ""), v.File).getAbsolutePath().replace('\\', '/'));
			
			cl.invokeClass("wombat.Wombat", new String[]{});
			
			
//			URLClassLoader cl = new URLClassLoader(new URL[]{new URL("file://" + prefs.get("install-directory", ""))});
//
//			Class<?> cls = Class.forName("wombat.Wombat", true, cl);
//			
//			System.out.println(">>> " + cls.getPackage() + ", " + cls.getName());
//			
//			cls.newInstance();
			
//			setVisible(false);
//			dispose();
			
		} catch(Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(this, "Error:\n" + e, "Unable to launch Wombat", JOptionPane.ERROR_MESSAGE);
		}
	}

	/**
	 * Install Wombat.
	 */
	void install() throws MalformedURLException, IOException {
		setVisible(true);
		
		JFileChooser chooser = new JFileChooser();
		chooser.setCurrentDirectory(new java.io.File("."));
		chooser.setDialogTitle("Install Wombat: Choose a directory");
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		chooser.setAcceptAllFileFilterUsed(false);

		if (chooser.showOpenDialog(this) != JFileChooser.APPROVE_OPTION) {
			log("No installation directory chosen.");
			return;
		}

		File dir = chooser.getCurrentDirectory();

		log("New installation to: " + dir.getAbsolutePath());
		prefs.put("install-directory", dir.getAbsolutePath());

		update();
		
		prefs.putBoolean("installed", true);
	}

	/**
	 * Check for and potentially update Wombat.
	 */
	void update() throws MalformedURLException, IOException {
		log("Checking for available updates");

		Map<String, Version> curVersions = parseVersions(prefs.get("versions", ""));
		Map<String, Version> newVersions = parseVersions(download(new URL(remoteDir + versionFile)));

		for (String name : newVersions.keySet()) {
			if (!curVersions.containsKey(name))
				log("Installing " + name + " at ("
						+ newVersions.get(name).Version + ")");
			else if (curVersions.get(name).compareTo(newVersions.get(name)) < 0)
				log("Updating " + name + " from "
						+ curVersions.get(name).Version + " to "
						+ newVersions.get(name).Version);
			else {
				log(name + " is up to date.");
				continue;
			}
			
			setVisible(true);

			download(new URL(remoteDir + newVersions.get(name).File), new File(
					prefs.get("install-directory", ""),
					newVersions.get(name).File));
			curVersions.put(name, newVersions.get(name));
		}

		StringBuilder sb = new StringBuilder();
		for (Version v : curVersions.values()) {
			sb.append(v.Name);
			sb.append(',');
			sb.append(v.Version);
			sb.append(',');
			sb.append(v.File);
			sb.append('\n');
		}
		prefs.put("versions", sb.toString());
	}

	/**
	 * Parse a version string.
	 * @param s The version string (common delimited, each line is Name,Version,File)
	 * @return A map of names to versions (see parameters)
	 */
	Map<String, Version> parseVersions(String s) {
		Map<String, Version> result = new HashMap<String, Version>();

		for (String line : s.split("\n")) {
			String[] parts = line.split(",");
			if (parts.length != 3)
				continue;

			result.put(parts[0], new Version(parts[0], parts[1], parts[2]));
		}

		return result;
	}

	/**
	 * Download a file into a string.
	 * @param from The source URL.
	 * @return The contents as a string.
	 */
	String download(URL from) throws IOException {
		StringBuilder out = new StringBuilder();
		BufferedInputStream in = new BufferedInputStream(from.openStream());

		int count;
		byte data[] = new byte[10240];
		while ((count = in.read(data, 0, 10240)) > 0)
			out.append(new String(data, 0, count));

		in.close();

		return out.toString();
	}

	/**
	 * Download a file to another file.
	 * @param from The source URL.
	 * @param to The destination file.
	 */
	void download(URL from, File to) throws IOException {
		BufferedOutputStream out = new BufferedOutputStream(
				new FileOutputStream(to));
		BufferedInputStream in = new BufferedInputStream(from.openStream());

		int count;
		byte data[] = new byte[10240];
		while ((count = in.read(data, 0, 10240)) > 0)
			out.write(data, 0, count);

		out.close();
		in.close();
	}

	/**
	 * Log any problems.
	 * @param msg The error to log.
	 */
	void log(String msg) {
		System.out.println(msg);
		progress.setText(progress.getText() + msg + "\n");
	}
}

/**
 * Represents version information.
 */
class Version implements Comparable<Version> {
	String Name;
	String File;
	String Version;
	List<Integer> parts;

	/**
	 * Create a new version record.
	 * @param name Name of the thing being updated.
	 * @param version The current version.
	 * @param file The filename that the version uses.
	 */
	public Version(String name, String version, String file) {
		Name = name;
		File = file;
		Version = version;

		parts = new ArrayList<Integer>();

		for (String s : version.split(".")) {
			try {
				parts.add(Integer.parseInt(s));
			} catch (NumberFormatException e) {
				parts.add((int) s.charAt(0));
			}
		}
	}

	/**
	 * Compare two versions.
	 */
	@Override
	public int compareTo(Version that) {
		for (int i = 0; i < Math.min(parts.size(), that.parts.size()); i++)
			if (parts.get(i) < that.parts.get(i))
				return -1;
			else if (parts.get(i) > that.parts.get(i))
				return 1;

		if (parts.size() < that.parts.size())
			return -1;
		else if (parts.size() > that.parts.size())
			return 1;
		else
			return 0;
	}
}

/**
 * A class loader for loading jar files, both local and remote.
 */
class JarClassLoader extends URLClassLoader {
    private URL url;

    /**
     * Creates a new JarClassLoader for the specified url.
     *
     * @param url the url of the jar file
     */
    public JarClassLoader(URL url) {
		super(new URL[] { url });
		this.url = url;
    }

    /**
     * Returns the name of the jar file main class, or null if
     * no "Main-Class" manifest attributes was defined.
     */
    public String getMainClassName() throws IOException {
		URL u = new URL("jar", "", url + "!/");
		JarURLConnection uc = (JarURLConnection)u.openConnection();
		Attributes attr = uc.getMainAttributes();
		return attr != null ? attr.getValue(Attributes.Name.MAIN_CLASS) : null;
    }
    
	/**
	 * Add a JAR file to the class loader.
	 * @param path The path of the JAR to load. 
	 */
	public void addFile(String path) throws MalformedURLException {
		String urlPath = "jar:file://" + path + "!/";
		addURL(new URL(urlPath));
	}

    /**
     * Invokes the application in this jar file given the name of the
     * main class and an array of arguments. The class must define a
     * static method "main" which takes an array of String arguemtns
     * and is of return type "void".
     *
     * @param name the name of the main class
     * @param args the arguments for the application
     * @exception ClassNotFoundException if the specified class could not
     *            be found
     * @exception NoSuchMethodException if the specified class does not
     *            contain a "main" method
     * @exception InvocationTargetException if the application raised an
     *            exception
     */
    public void invokeClass(String name, String[] args) throws ClassNotFoundException, NoSuchMethodException, InvocationTargetException
    {
		Class<?> c = loadClass(name);
		Method m = c.getMethod("main", new Class[] { args.getClass() });
		m.setAccessible(true);
		int mods = m.getModifiers();
		if (m.getReturnType() != void.class || !Modifier.isStatic(mods) ||
		    !Modifier.isPublic(mods)) {
		    throw new NoSuchMethodException("main");
		}
		try {
		    m.invoke(null, new Object[] { args });
		} catch (IllegalAccessException e) {
		    // This should not happen, as we have disabled access checks
		}
    }

}