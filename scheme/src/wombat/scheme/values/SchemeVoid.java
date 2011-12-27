package wombat.scheme.values;

/**
 * Void object. Returned when the return isn't specified.
 */
public class SchemeVoid extends SchemeObject<Object> {
	private static final long serialVersionUID = 1039101308536892172L;

	/**
	 * Create a new void object.
	 */
	public SchemeVoid() {
		super(null);
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		return "#<void>";
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		return "#<void>";
	}

}
