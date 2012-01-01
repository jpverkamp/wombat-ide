package wombat.scheme.errors;

import wombat.scheme.values.*;

public class SchemeSyntaxError extends SchemeError {
	private static final long serialVersionUID = -6898709884346328744L;

	/**
	 * Create a syntax error.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeSyntaxError(SchemeObject<?> src, String msg) {
		super(src, msg);
	}
}
