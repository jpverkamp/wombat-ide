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
	
	public String getMessage() {
		String sourceName = Source.display();
		if (sourceName.length() > 40)
			sourceName = sourceName.substring(0, 40) + "...";
		
		return "Error in " + sourceName + " at " + Source.getLocation() + "\n" + super.getMessage(); 
	}
}
