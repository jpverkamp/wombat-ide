package util.errors;

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