package util;

import java.io.PrintStream;

public class OutputIntercept extends PrintStream {
	static PrintStream console = System.out;
	static PrintStream err = System.err;
	
	static OutputIntercept me = new OutputIntercept();
	static boolean enabled = false;
	
	static StringBuilder buffer = new StringBuilder();
	static boolean content = false;
	
	/** Suppress constructor. */
	private OutputIntercept() { 
		super(console, true); 
	}
	
	/**
	 * Enable output intercepting.
	 */
	public static void enable() {
		enabled = true;
		
		System.out.println("About to capture out and err.");
		
		System.setOut(me);
		System.setErr(me);
	}
	
	/**
	 * Disable output intercepting.
	 */
	public static void disable() {
		enabled = false;
		
		System.out.println("About to return out and err to normal.");
		
		System.setOut(console);
		System.setErr(err);
	}
	
	/**
	 * If there is content currently in the buffer.
	 * @return True/false.
	 */
	public static boolean hasContent() {
		return content;
	}
	
	/**
	 * Get the current buffer contents.
	 * @return The current buffer contents.
	 */
	public static String getContent() {
		if (content) {
			String result = buffer.toString();
			
			buffer.delete(0, buffer.length());
			content = false;
			
			return result;
		}
		else {
			return "";
		}
	}

	/**
	 * Print a string.
	 */
	public void print(String x) {
		console.print(x);
		buffer.append(x);
		content = true;
	}

	/**
	 * Print a string then a newline.
	 */
	public void println(String x) {
		console.println(x);
		buffer.append(x);
		buffer.append("\n");
		content = true;
	}

	/**
	 * Print an object.
	 */
	public void print(Object x) {
		console.print(x);
		buffer.append(x.toString());
		content = true;
	}

	/**
	 * Print an object then a newline.
	 */
	public void println(Object x) {
		console.println(x);
		buffer.append(x.toString());
		buffer.append("\n");
		content = true;
	}
}