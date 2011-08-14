package util;

import gui.ErrorFrame;

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
		
		ErrorFrame.log("Enabling OutputIntercept.");
		
		System.setOut(me);
		System.setErr(me);
	}
	
	/**
	 * Disable output intercepting.
	 */
	public static void disable() {
		enabled = false;
		
		ErrorFrame.log("Disabling OutputIntercept.");
		
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
		go(x);
	}

	/**
	 * Print a string then a newline.
	 */
	public void println(String x) {
		go(x + "\n");
	}

	/**
	 * Print an object.
	 */
	public void print(Object x) {
		go(x.toString());
	}

	/**
	 * Print an object then a newline.
	 */
	public void println(Object x) {
		go(x.toString() + "\n");
	}
	
	private void go(String x) {
//		console.print(x);
		buffer.append(x);
		content = true;
	}
}