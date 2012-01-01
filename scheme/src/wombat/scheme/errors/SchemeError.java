package wombat.scheme.errors;

import wombat.scheme.values.*;

/**
 * Generic Scheme Error.
 */
public abstract class SchemeError extends Error {
	private static final long serialVersionUID = -7579375473634586976L;
	
	SchemeObject<?> Source;
	
	/**
	 * Create an error.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeError(SchemeObject<?> src, String msg) {
		super(msg);
		Source = src;
	}
}
