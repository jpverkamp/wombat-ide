package errors;

import values.*;

/**
 * Generic Scheme Error.
 */
public abstract class SchemeError extends Error {
	private static final long serialVersionUID = -7579375473634586976L;
	
	SchemeObject<?> Source;
	SchemeContinuation Stack;
	
	/**
	 * Create an Error without a stack trace.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeError(SchemeObject<?> src, String msg) {
		this(src, null, msg);
	}
	
	/**
	 * Create an Error with a stack trace.
	 * 
	 * @param src The object that cause the Error.
	 * @param k A continuation representing the stack trace.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeError(SchemeObject<?> src, SchemeContinuation k, String msg) {
		super(msg);
		Source = src;
		Stack = k;
	}
}
