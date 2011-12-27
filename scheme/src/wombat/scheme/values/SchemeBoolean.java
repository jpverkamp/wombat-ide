package wombat.scheme.values;

/**
 * True or false.
 */
public class SchemeBoolean extends SchemeObject<Boolean> {
	private static final long serialVersionUID = -1949643887170508447L;
	
	/**
	 * Create a new boolean.
	 * @param value True or false.
	 */
	public SchemeBoolean(Boolean value) {
		super(value);
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		return write();
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		if (Value)
			return "#t";
		else
			return "#f";
	}

}
