/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.util.errors;

import java.util.ArrayList;
import java.util.List;

/**
 * Manage errors by relaying them to any number of error listeners.
 */
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

