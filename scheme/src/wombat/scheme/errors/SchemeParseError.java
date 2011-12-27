package wombat.scheme.errors;

import wombat.scheme.values.SchemeObject;

public class SchemeParseError extends SchemeError {
	private static final long serialVersionUID = -6327468641560922995L;

	/**
	 * Create a parse Error (parse Errors never have a stack trace).
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 */
	public SchemeParseError(SchemeObject<?> src, String msg) {
		super(src, msg);
	}
}
