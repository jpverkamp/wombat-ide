/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.util.errors;

/**
 * Listen for errors.
 */
public interface ErrorListener {
	/**
	 * Log an error.
	 * @param msg The error.
	 */
	public void logError(String msg);
}