package wombat.scheme.errors;

import wombat.scheme.values.SchemeContinuation;
import wombat.scheme.values.SchemeObject;

public class SchemeSyntaxError extends SchemeError {
	private static final long serialVersionUID = -6898709884346328744L;

	/**
	 * Create a syntax Error without a stack trace.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeSyntaxError(SchemeObject<?> src, String msg) {
		super(src, msg);
	}
	
	/**
	 * Create a syntax Error with a stack trace.
	 * 
	 * @param src The object that cause the Error.
	 * @param k A continuation representing the stack trace.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeSyntaxError(SchemeObject<?> src, SchemeContinuation k, String msg) {
		super(src, k, msg);
	}
}
