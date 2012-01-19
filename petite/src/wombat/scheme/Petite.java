package wombat.scheme;

import java.io.*;
import java.util.*;

/**
 * Class to wrap Petite bindings.
 */
public class Petite {
	/**
	 * Run from the command line, providing a REPL.
	 * @param args Ignored.
	 */
	public static void main(String[] args) {
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
	
    boolean JustSent = false;
    boolean Ready = false;
    StringBuffer Buffer = new StringBuffer();
    Writer ToPetite;
    Process NativeProcess;
	
    /**
     * Create a new Petite thread.
     * @throws IOException If we fail to access the Petite process.
     */
	public Petite() throws IOException {
		File cd = new File(".").getAbsoluteFile();
		File lib = new File(cd, "lib");

		// Find the correct Petite directory.
		File pdir = null;
		for (String path : lib.list())
			if (path.startsWith("petite") && path.endsWith(IsWindows ? "win" : IsOSX ? "osx" : IsLinux ? "linux" : "unknown"))
				pdir = new File(lib, path);
		if (pdir == null)
			throw new Error("Unable to locate Petite directory.");
		
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
		
		// Create a listener thread.
		Thread fromPetiteThread = new Thread(new Runnable() {
			public void run() {
				Reader r = new InputStreamReader(NativeProcess.getInputStream());

				char thisc = '\0', lastc = '\0';
				while (true) {
					try {
						
						while (true) {
							lastc = thisc;
							thisc = (char) r.read();
							
							if ((JustSent || lastc == '\n') && thisc == '>')
								Ready = true;
							else
								Buffer.append(thisc);
							
							JustSent = false;
						}
						
					} catch(Exception e) {
						System.err.println("Petite buffer is broken");
						Buffer.append("\nException: Petite buffer is broken\n");
					}
				}
			}
		});
		fromPetiteThread.setDaemon(true);
		fromPetiteThread.start();
		
		Thread t = new Thread(new Runnable() {
			public void run() {
				while (true) {
					try { Thread.sleep(10000); } catch (InterruptedException e) {}
					
					System.err.println("trying to stop");
					stop();
				}
			}
		});
		t.setDaemon(true);
		t.start();
	}
	
	/**
	 * Reset Petite's environment.
	 */
	public void reset() {
		sendCommand("(interaction-environment (copy-environment (scheme-environment) #t))");
//		sendCommand("(waiter-prompt-string \"%\"");
	}
	
	/**
	 * Stop the currently running command.
	 */
	public void stop() {
		try {
			ToPetite.write(0x1d);
			ToPetite.flush();
		} catch (IOException e) {
		}
	}
	
	/**
	 * Check if the process is ready.
	 * @return If the process is ready.
	 */
	public boolean isReady() {
		return Ready;
	}
	
	/**
	 * If the output buffer has any content.
	 * @return True or false.
	 */
	public boolean hasOutput() {
		return Buffer.length() > 0;
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
			
			JustSent = true;
			Ready = false;
			
		} catch(Exception e) {
			System.err.println("Unable to execute command");
			Buffer.append("\nException: Unable to execute command\n");
		}
	}
}
