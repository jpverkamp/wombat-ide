package util;

import java.util.ArrayList;
import java.util.List;

public class ErrorManager {
	private ErrorManager() {}
	
	static List<ErrorListener> listeners = new ArrayList<ErrorListener>();
	
	/**
	 * Add an error listener.
	 * @param el The error listener.
	 */
	public static void addErrorListener(ErrorListener el) {
		listeners.add(el);
	}
	
	/**
	 * Log an error.
	 * @param msg The error.
	 */
	public static void logError(String msg) {
		for (ErrorListener el : listeners)
			el.logError(msg);
	}
}

