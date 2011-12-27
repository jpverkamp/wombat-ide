package wombat.scheme.values;

/**
 * Creates a procedure.
 */
public abstract class SchemeProcedure extends SchemeObject<Object> {
	private static final long serialVersionUID = 6809643481710108526L;
	
	String Name;
	
	/**
	 * Create a new anonymous procedure.
	 */
	public SchemeProcedure() {
		super(null);
	}
	
	/**
	 * Create a new named procedure.
	 * @param name
	 */
	public SchemeProcedure(String name) {
		super(null);
		Name = name;
	}
	
	
	/**
	 * Apply the procedure.
	 * 
	 * @param args Arguments to the function.
	 * @return The result of applying the function.
	 */
	public abstract SchemeObject<?> apply(SchemeObject<?>... args);
	
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
		if (Name == null)
			return "#<procedure>";
		else
			return "#<procedure " + Name + ">";
	}
}
