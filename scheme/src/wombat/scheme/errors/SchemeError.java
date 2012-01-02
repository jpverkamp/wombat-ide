package wombat.scheme.errors;

import wombat.scheme.values.*;

/**
 * Generic Scheme Error.
 */
public abstract class SchemeError extends Error {
	private static final long serialVersionUID = -7579375473634586976L;
	
	SchemeObject<?> Source;
	SchemeObject<?>[] Irritants;
	
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
	
	/**
	 * Create an error with irritants.
	 * 
	 * @param src The object that caused the Error.
	 * @param msg A (hopefully) helpful error message.
	 * @param irritants Any other objects that might help debugging.
	 */
	public SchemeError(SchemeProcedure src, String msg, SchemeObject<?>[] irritants) {
		super(msg);
		Source = src;
		Irritants = irritants;
	}

	/**
	 * Format the error message.
	 */
	public String getMessage() {
		StringBuilder msg = new StringBuilder();
		msg.append("Error in ");
		
		String sourceName = Source.display();
		if (sourceName.length() > 40)
			sourceName = sourceName.substring(0, 40) + "...";
		msg.append(sourceName);
		msg.append("\n");
		
		msg.append(super.getMessage());
		
		if (Irritants != null && Irritants.length > 0) {
			msg.append("\nIrritants: ");
			for (int i = 0; i < Irritants.length; i++) {
				msg.append(Irritants[i].display());
				msg.append(", ");
			}
			msg.delete(msg.length() - 2, msg.length());
		}
		
		return msg.toString();
	}
}
