package wombat.scheme.values;

/**
 * Void object. Returned when the return isn't specified.
 */
public class SchemeVoid extends SchemeObject<Object> {
	private static final long serialVersionUID = 1039101308536892172L;

	static final SchemeVoid Singleton = new SchemeVoid();
	
	/**
	 * Create a new void object.
	 */
	private SchemeVoid() {
		super(null);
	}
	
	/**
	 * Get the void.
	 * @return The void.
	 */
	public static SchemeVoid singleton() {
		return Singleton;
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
