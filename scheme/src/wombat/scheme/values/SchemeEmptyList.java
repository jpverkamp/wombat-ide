package wombat.scheme.values;

/**
 * Empty list.
 */
public class SchemeEmptyList extends SchemeObject<Object> {
	private static final long serialVersionUID = 5537628395539291741L;
	
	static final SchemeEmptyList Singleton = new SchemeEmptyList();

	/**
	 * Create a new empty list.
	 */
	private SchemeEmptyList() {
		super(null);
	}
	
	/**
	 * Return the empty list.
	 * @return The empty list.
	 */
	public static SchemeEmptyList singleton() {
		return Singleton;
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		return "()";
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		return "()";
	}

	/**
	 * Check if equal.
	 */
	public boolean equals(Object other) {
		return other instanceof SchemeEmptyList;
	}
}
