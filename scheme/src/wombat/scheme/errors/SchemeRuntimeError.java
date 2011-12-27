package wombat.scheme.errors;

import wombat.scheme.values.SchemeContinuation;
import wombat.scheme.values.SchemeObject;

public class SchemeRuntimeError extends SchemeError {
	private static final long serialVersionUID = 297969606443356635L;
	
	/**
	 * Create a runtime Error without a stack trace.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeRuntimeError(SchemeObject<?> src, String msg) {
		super(src, msg);
	}
	
	/**
	 * Create a runtime Error with a stack trace.
	 * 
	 * @param src The object that cause the Error.
	 * @param k A continuation representing the stack trace.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeRuntimeError(SchemeObject<?> src, SchemeContinuation k, String msg) {
		super(src, k, msg);
	}
}
