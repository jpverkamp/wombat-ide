package util;

import gnu.mapping.OutPort;

import java.io.StringWriter;

public class OutputIntercept extends OutPort {
	static OutputIntercept me = new OutputIntercept();
	static boolean enabled = false;
	
	static StringBuilder buffer = new StringBuilder();
	static boolean content = false;
	
	/** Suppress constructor. */
	private OutputIntercept() {
		super(new StringWriter());
		System.out.println("created");
	}
	
	/**
	 * Accessor 
	 * @return me
	 */
	public static OutputIntercept me() {
		if (me == null)
			me = new OutputIntercept();
		return me;
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

	public void print(int v) { print("" + toString()); }
	public void print(long v) { print("" + toString()); }
	public void print(double v) { print("" + toString()); }
	public void print(float v) { print("" + toString()); }
	public void print(boolean v) { print("" + toString()); }
	public void print(Object v) { print(v.toString()); }
	
	public void print(String v) {
		System.out.println("write: " + v);
		content = true;
		buffer.append(v);
	}

}