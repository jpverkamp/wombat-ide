package wombat.scheme.errors;

import wombat.scheme.values.*;

public class SchemeRuntimeError extends SchemeError {
	private static final long serialVersionUID = 297969606443356635L;
	
	/**
	 * Create a runtime error.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeRuntimeError(SchemeObject<?> src, String msg) {
		super(src, msg);
	}
}
