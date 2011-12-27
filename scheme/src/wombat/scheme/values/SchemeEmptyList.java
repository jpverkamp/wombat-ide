package wombat.scheme.values;

/**
 * Empty list.
 */
public class SchemeEmptyList extends SchemeObject<Object> {
	private static final long serialVersionUID = 5537628395539291741L;

	/**
	 * Create a new empty list.
	 */
	public SchemeEmptyList() {
		super(null);
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

}
