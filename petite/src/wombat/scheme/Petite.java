package wombat.scheme;

import java.io.*;
import java.net.URISyntaxException;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

/**
 * Class to wrap Petite bindings.
 */
public class Petite {
	/**
	 * Run from the command line, providing a REPL.
	 * @param args Ignored.
	 * @throws URISyntaxException If we botched the files from the JAR.
	 */
	public static void main(String[] args) throws URISyntaxException {
		try {
			final Petite p = new Petite();
			final Scanner s = new Scanner(System.in);
			
			Thread t = new Thread(new Runnable() {
				public void run() {
					while (true) {
						if (p.hasOutput()) {
							System.out.println(p.getOutput());
							System.out.print(">> ");
						}
						
						try { Thread.sleep(10); } catch (InterruptedException e) {}
					}
				}
			});
			t.setDaemon(true);
			t.start();
			
			while (true) {
				if (p.isReady() && s.hasNextLine()) {
					String cmd = s.nextLine();
					p.sendCommand(cmd);
				}
				
				try { Thread.sleep(10); } catch (InterruptedException e) {}
			}
			
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	static final String os = System.getProperty("os.name").toLowerCase();
    static final boolean IsWindows = os.indexOf("win") != -1;
    static final boolean IsOSX = os.indexOf("mac") != -1;
    static final boolean IsLinux = (os.indexOf("nux") != -1) || (os.indexOf("nix") != -1);
    
    static final char Promt1 = '|';
    static final char Promt2 = '`';
	
    boolean Starting;
    boolean SeenPromt1;
    boolean Ready;
    StringBuffer Buffer;
    
    Writer ToPetite;
    Reader FromPetite;
    Process NativeProcess;
	
    /**
     * Create a new Petite thread.
     * @throws IOException If we fail to access the Petite process.
     * @throws URISyntaxException If we have problems getting the path from a JAR file.
     */
	public Petite() throws IOException, URISyntaxException {
		connect();
	}
	
	/**
	 * (Re)connect the Petite process.
	 * @throws IOException If we couldn't connect.
	 * @throws URISyntaxException If we have problems getting the path from a JAR file.
	 */
	private void connect() throws IOException, URISyntaxException {
	    // Reset the wrapper state (necessary in the case of a reconnect).
		Starting = true;
		SeenPromt1 = false;
	    Ready = false;
	    if (Buffer == null)
	    	Buffer = new StringBuffer();
	    else 
	    	Buffer.delete(0, Buffer.length());

	    // The root is either this directory or a nested 'lib' directory.
	    File[] searchDirs = new File[]{
	    		new File("").getCanonicalFile(),
	    		new File(new File("").getCanonicalFile(), "lib").getCanonicalFile(),
	    		new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getCanonicalFile(),
	    		new File(new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()), "lib").getCanonicalFile()
	    };
	    
	    // Find the correct Petite directory.
		File pdir = null;
		for (File dir : searchDirs)
			if (dir != null && dir.exists() && dir.isDirectory())
				for (String path : dir.list())
					if (path.startsWith("petite") && path.endsWith(IsWindows ? "win" : IsOSX ? "osx" : IsLinux ? "linux" : "unknown"))
						pdir = new File(dir, path);

		
		// If it didn't we may have to look for zip files.
		if (pdir == null) {
			for (File dir : searchDirs) {
				if (dir != null && dir.exists() && dir.isDirectory()) {
					for (String path : dir.list()) {
						if (path.startsWith("petite") && path.endsWith((IsWindows ? "win" : IsOSX ? "osx" : IsLinux ? "linux" : "unknown") + ".zip")) {
							ZipFile zip = new ZipFile(new File(dir, path));
							
							@SuppressWarnings("unchecked")
							Enumeration<ZipEntry> entries = (Enumeration<ZipEntry>) zip.entries();
	
							while (entries.hasMoreElements()) {
						        ZipEntry entry = entries.nextElement();
	
						        if (entry.isDirectory()) {
						        	System.out.println("unzipping directory:\n" + entry.getName() + "\nto:\n" + new File(dir, entry.getName()));
						        	new File(dir, entry.getName()).getCanonicalFile().mkdirs();
						        } else {
						        	System.out.println("unzipping:\n" + entry.getName() + "\nto:\n" + new File(dir, entry.getName()));
						        	
						        	new File(dir, entry.getName()).getCanonicalFile().getParentFile().mkdirs();
						        	
						        	InputStream in = zip.getInputStream(entry);
						        	OutputStream out = new BufferedOutputStream(new FileOutputStream(new File(dir, entry.getName())));
						        	
						        	byte[] buffer = new byte[1024];
						            int len;
	
						            while((len = in.read(buffer)) >= 0)
						              out.write(buffer, 0, len);
	
						            in.close();
						            out.close();
						        }
						      }
	
						      zip.close();
						      
						      // Try again.
						      connect();
						      return;
						}
					}
				}
			}
		}
		
		// If we made it this far without a directory, something broke. :(
		if (pdir == null)
			throw new IOException("Unable to find Petite directory.");
			
		
		// Create the process builder.
		ProcessBuilder pb = new ProcessBuilder(
				new File(pdir, (IsWindows ? "petite.exe" : "petite")).getAbsolutePath(), 
				"-b", 
				new File(pdir, "petite.boot").getAbsolutePath()
			);
		pb.directory(pdir);
		pb.redirectErrorStream(true);
		
		// Start the process.
		NativeProcess = pb.start();
		
		// Set up the print writer.
		ToPetite = new PrintWriter(NativeProcess.getOutputStream());
		FromPetite = new InputStreamReader(NativeProcess.getInputStream());
		
		// Immediately send the command to reset the prompt.
		sendCommand("(waiter-prompt-string \"|`\")");
		
		// Create a listener thread.
		Thread fromPetiteThread = new Thread() {
			public void run() {
				char c;
				while (true) {
					try {
						
						while (true) {
							c = (char) FromPetite.read();
							
							// Potential start of a prompt.
							if (c == Promt1) {
								SeenPromt1 = true;
							}
							
							// The whole prompt.
							else if (SeenPromt1 && c == Promt2) {
								SeenPromt1 = false;
								
								if (Starting) {
									Buffer.delete(0, Buffer.length());
									Starting = false;
								}
								
								Ready = true;
							}
							
							// Thought it was a prompt, but we were wrong.
							// Remember to store the first half of the prompt.
							else if (SeenPromt1) {
								SeenPromt1 = false;
								Buffer.append(Promt1);
								Buffer.append(c);
							}
							
							// Normal case, no new characters.
							else {
								Buffer.append(c);
							}
						}
						
					} catch(Exception e) {
						System.err.println("Petite buffer is broken");
						Buffer.append("\nException: Petite buffer is broken\n");
					}
				}
			}
		};
		fromPetiteThread.setDaemon(true);
		fromPetiteThread.start();
		
		Runtime.getRuntime().addShutdownHook(new Thread() {
			public void run() {
				NativeProcess.destroy();
			}
		});
	}
	
	/**
	 * Reset Petite's environment.
	 */
	public void reset() {
		sendCommand("(interaction-environment (copy-environment (scheme-environment) #t))");
	}
	
	/**
	 * Check if the process is ready.
	 * @return If the process is ready.
	 */
	public boolean isReady() {
		return Ready;
	}
	
	/**
	 * Stop the running process and get a new one.
	 * @throws IOException If we cannot connect.
	 * @throws URISyntaxException Botched file from JAR.
	 */
	public void stop() throws IOException, URISyntaxException {
	    // Shut down the old connection.
		Ready = false;
	    NativeProcess.destroy();
		
	    // Reconnect.
		connect();
	}
	
	/**
	 * If the output buffer has any content.
	 * @return True or false.
	 */
	public boolean hasOutput() {
		return !Starting && (Buffer.length() > 0);
	}
	
	/**
	 * Get the contents of the output buffer.
	 * @return The output buffer.
	 */
	public String getOutput() {
		String output = Buffer.toString();
		Buffer.delete(0, Buffer.length());
		return output;
	}
	
	/**
	 * Send a command to the Petite process.
	 * @param cmd The command to send.
	 */
	public void sendCommand(String cmd) {
		try {
			
			ToPetite.write(cmd);
			if (!cmd.endsWith("\n"))
				ToPetite.write("\n");
			ToPetite.flush();
			
			Ready = false;
			
		} catch(Exception e) {
			System.err.println("Unable to execute command");
			Buffer.append("\nException: Unable to execute command\n");
		}
	}

	
}
