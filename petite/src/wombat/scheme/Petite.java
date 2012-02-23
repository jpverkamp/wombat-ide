package wombat.scheme;

import java.io.*;
import java.net.URISyntaxException;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import wombat.scheme.util.InteropAPI;

/**
 * Class to wrap Petite bindings.
 */
public class Petite {
	static final boolean DEBUG_INTEROP = false;
	
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
			
			while (p.isRunning()) {
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
    
    static final char Prompt1 = '|';
    static final char Prompt2 = '`';
    static final char Interop = '!';
    
    boolean Running;
    boolean Starting;
    boolean SeenPrompt1;
    boolean Ready;
    
    boolean InInterop;
    
    StringBuffer Buffer;
    StringBuffer InteropBuffer;
    Lock BufferLock;
    
    Writer ToPetite;
    Reader FromPetite;
    Process NativeProcess;
    
    Thread FromPetiteThread;
	

	    // The root is either this directory or a nested 'lib' directory.
    static final File[] searchDirs = new File[]{
	new File("").getCanonicalFile(),
	new File(new File("").getCanonicalFile(), "lib").getCanonicalFile(),
	new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getCanonicalFile(),
	new File(new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()), "lib").getCanonicalFile(),
	new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile().getCanonicalFile(),
	new File(new File(Petite.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath()).getParentFile(), "lib").getCanonicalFile(),
    };

    /**
     * Create a new Petite thread.
     * @throws IOException If we fail to access the Petite process.
     * @throws URISyntaxException If we have problems getting the path from a JAR file.
     */
	public Petite() throws IOException, URISyntaxException {
	    // Unzip the c211 library.
	    unzipC211Lib();

	    // Connect to an initial Petite session.
	    connect();
	}
	
	/**
	 * (Re)connect the Petite process.
	 * @throws IOException If we couldn't connect.
	 * @throws URISyntaxException If we have problems getting the path from a JAR file.
	 */
	private void connect() throws IOException, URISyntaxException {
	    System.err.println("Petite connecting");
		
	    // Reset the wrapper state (necessary in the case of a reconnect).
	    Starting = true;
	    SeenPrompt1 = false;
	    Ready = false;
	    Running = true;
	    InInterop = false;
	    
	    if (Buffer == null)
	    	Buffer = new StringBuffer();
	    else 
	    	Buffer.delete(0, Buffer.length());
	    
	    if (InteropBuffer == null)
	    	InteropBuffer = new StringBuffer();
	    else
	    	InteropBuffer.delete(0,  InteropBuffer.length());
	    
	    BufferLock = new ReentrantLock();
	    
	    // Find the correct Petite directory.
		File pdir = null;
		for (File dir : searchDirs) {
		    System.out.println("Looking in " + dir + " for petite.");
		    if (dir != null && dir.exists() && dir.isDirectory()) {
			for (String path : dir.list()) {
			    if (path.startsWith("petite") && path.endsWith(IsWindows ? "win" : IsOSX ? "osx" : IsLinux ? "linux" : "unknown")) {
				pdir = new File(dir, path);
				System.out.println("Found: " + pdir);
			    }
			}
		    }
		}
		
		// If it didn't we may have to look for zip files.
		if (pdir == null) {
		    System.out.println("Petite not find, looking for an archive.");
		    for (File dir : searchDirs) {
		    System.out.println("Looking in " + dir + " for petite archive.");
			if (dir != null && dir.exists() && dir.isDirectory()) {
			    for (String path : dir.list()) {
				if (path.startsWith("petite") && path.endsWith((IsWindows ? "win" : IsOSX ? "osx" : IsLinux ? "linux" : "unknown") + ".zip")) {
				    ZipFile zip = new ZipFile(new File(dir, path));
				    System.out.println("Found: " + zip);
				    
				    @SuppressWarnings("unchecked")
				    Enumeration<ZipEntry> entries = (Enumeration<ZipEntry>) zip.entries();
				    
				    while (entries.hasMoreElements()) {
					ZipEntry entry = entries.nextElement();
					
					if (entry.isDirectory()) {
					    System.out.println("\tunzipping: " + entry.getName());
					    new File(dir, entry.getName()).getCanonicalFile().mkdirs();
					} else {
					    System.out.println("\tunzipping: " + entry.getName());
					    new File(dir, entry.getName()).getCanonicalFile().getParentFile().mkdirs();
					    
					    File targetFile = new File(dir, entry.getName());
					    InputStream in = zip.getInputStream(entry);
					    OutputStream out = new BufferedOutputStream(new FileOutputStream(targetFile));
					    
					    byte[] buffer = new byte[1024];
					    int len;
					    
					    while((len = in.read(buffer)) >= 0)
						out.write(buffer, 0, len);
					    
					    in.close();
					    out.close();
					    
					    targetFile.setExecutable(true);
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
				new File(pdir, "petite.boot").getAbsolutePath(),
				"--libdirs",
				"lib"
			);
		pb.directory(pdir.getParentFile().getParentFile());
		pb.redirectErrorStream(true);
		
		// Start the process.
		NativeProcess = pb.start();
		
		// Set up the print writer.
		ToPetite = new PrintWriter(NativeProcess.getOutputStream());
		FromPetite = new InputStreamReader(NativeProcess.getInputStream());
		
		// Immediately send the command to reset the prompt.
		reset();
		
		// Create a listener thread.
		if (FromPetiteThread == null) {
			FromPetiteThread = new Thread() {
				public void run() {
					char c;
					try {
						while (true) {
							c = (char) FromPetite.read();
							BufferLock.lock();
							
							// Close down on end of file.
							if (c == (char) 65535) {
								Running = false;
								break;
							}
							
							// Potential start of a prompt.
							else if (c == Prompt1) {
								SeenPrompt1 = true;
							}
							
							// The whole prompt.
							else if (SeenPrompt1 && c == Prompt2) {
								SeenPrompt1 = false;
								
								if (Starting) {
									Buffer.delete(0, Buffer.length());
									Starting = false;
								}
								
								Ready = true;
							} 
							
							// Switch to interop mode on Prompt1 + Interop
							else if (SeenPrompt1 && c == Interop) {
								SeenPrompt1 = false;
								
								if (InInterop) {
									String[] parts = InteropBuffer.toString().split(" ", 2);
									String key = parts[0];
									String val = (parts.length > 1 ? parts[1] : null);
									
									if (DEBUG_INTEROP) System.out.println("calling interop: " + key + " with " + val); // debug
									String result = InteropAPI.interop(key, val);
									if (DEBUG_INTEROP) System.out.println("interop returns: " + (result.length() > 10 ? result.subSequence(0,  10) + "..." : result)); // debug
									if (result != null) {
										ToPetite.write(result + " ");
										ToPetite.flush();
									}
									
									if (DEBUG_INTEROP) System.out.println("exiting interop");
									Ready = true;
										
									InteropBuffer.delete(0, InteropBuffer.length());
									InInterop = false;
								} else {
									if (DEBUG_INTEROP) System.out.println("entering interop"); // debug
									
									InInterop = true;
								}
								
							}
							
							// Thought it was a prompt, but we were wrong.
							// Remember to store the first half of the prompt.
							else if (SeenPrompt1) {
								SeenPrompt1 = false;
										
								if (InInterop) {
									InteropBuffer.append(Prompt1);
									InteropBuffer.append(c);
								} else {
									Buffer.append(Prompt1);
									Buffer.append(c);
								}
							}
							
							// Normal case, no new characters.
							else {
								if (InInterop) {
									InteropBuffer.append(c);
								} else {
									Buffer.append(c);
								}
							}
							
							BufferLock.unlock();
						}
						
					} catch(Exception e) {
						System.err.println("Petite buffer is broken");
						Buffer.append("\nException: Petite buffer is broken\n");

						// If we get here, Petite has collapsed.
						// Destroy the connected process and restart.
						try { restart(); } catch (Exception e2) { }
					}
				}
			};
			FromPetiteThread.setDaemon(true);
			FromPetiteThread.start();
		}
		
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
		// Actually clear the environment
		sendCommand("(interaction-environment (copy-environment (scheme-environment) #t))");
		
		// Make sure that the prompt is set as we want it
		sendCommand("(waiter-prompt-string \"|`\")");
		
		// So that (eq? 'A 'a) => #t
		sendCommand("(case-sensitive #f)");
		
		// So that gensyms look at least semi-sane (it's not like anyone will need them)
		sendCommand("(print-gensym #f)");
		
		// To test infinite loops
		sendCommand("(define (omega) ((lambda (x) (x x)) (lambda (x) (x x))))");

		// Fix error message that give define/lambda names
		sendCommand("(import (wombat define))");
	}
	
	/**
	 * Check if the process is ready.
	 * @return If the process is ready.
	 */
	public boolean isReady() {
		return Ready;
	}
	
	/**
	 * Check if the process is still running.
	 * @return If the process is still running.
	 */
	public boolean isRunning() {
		return Running;
	}
	
	/**
	 * Stop the running process.
	 * @throws IOException If we cannot connect.
	 * @throws URISyntaxException Botched file from JAR.
	 */
	public void stop() throws IOException, URISyntaxException {
		System.err.println("Petite stopping");
		
	    // Shut down the old connection.
		Ready = false;
		BufferLock.lock();
		Buffer.delete(0, Buffer.length());
		BufferLock.unlock();
	    NativeProcess.destroy();
	}

	/** 
	 * Stop and restart.
	 * @throws IOException If we cannot connect.
	 * @throws URISyntaxException Botched file from JAR.
	 */
	public void restart() throws IOException, URISyntaxException {
		System.err.println("Petite restarting");
		
		stop();
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
		BufferLock.lock();
		String output = Buffer.toString();
		Buffer.delete(0, Buffer.length());
		BufferLock.unlock();
		return output;
	}
	
	/**
	 * Send a command to the Petite process.
	 * @param cmd The command to send.
	 */
	public void sendCommand(String cmd) {
		try {
			
			// Swap out lambda character for string
			cmd = cmd.replace("\u03BB", "lambda");
			
			// Send it, make sure there's a newline.
			// Use flush to force it.
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

    /**
     * Unpack the C211 library.
     */
    void unzipC211Lib() {
	File libZip;
	for (File dir : searchDirs) {
	    if (dir.

	ZipFile zip = new ZipFile(new File(dir, path));
	System.out.println("Found: " + zip);
				    
	@SuppressWarnings("unchecked")
	    Enumeration<ZipEntry> entries = (Enumeration<ZipEntry>) zip.entries();
				    
	while (entries.hasMoreElements()) {
	    ZipEntry entry = entries.nextElement();
					
	    if (entry.isDirectory()) {
		System.out.println("\tunzipping: " + entry.getName());
		new File(dir, entry.getName()).getCanonicalFile().mkdirs();
	    } else {
		System.out.println("\tunzipping: " + entry.getName());
		new File(dir, entry.getName()).getCanonicalFile().getParentFile().mkdirs();
					    
		File targetFile = new File(dir, entry.getName());
		InputStream in = zip.getInputStream(entry);
		OutputStream out = new BufferedOutputStream(new FileOutputStream(targetFile));
					    
		byte[] buffer = new byte[1024];
		int len;
					    
		while((len = in.read(buffer)) >= 0)
		    out.write(buffer, 0, len);
					    
		in.close();
		out.close();
					    
		targetFile.setExecutable(true);
	    }
	}
				    
	zip.close();

    }
}
