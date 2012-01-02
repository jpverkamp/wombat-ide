package wombat.scheme.errors;

import wombat.scheme.values.SchemeObject;

public class SchemeNotImplementedError extends SchemeError {
	private static final long serialVersionUID = 3999524218903390155L;

	/**
	 * Create an error signifying that the given source object has not been fully implemented.
	 * 
	 * @param src The object that caused the Error.
	 */
	public SchemeNotImplementedError(SchemeObject<?> src) {
		super(src, "Not implemented");
	}
}
