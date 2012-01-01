package wombat.scheme.values;

import wombat.scheme.SExpression;

/**
 * Creates a procedure.
 */
public abstract class SchemeMacro extends SchemeProcedure {
	private static final long serialVersionUID = -6659564716921278661L;

	/**
	 * Create a new anonymous macro.
	 */
	public SchemeMacro() {
		super(null);
	}
	
	/**
	 * Create a new named macro.
	 * @param name
	 */
	public SchemeMacro(String name) {
		super(name);
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		if (Name == null)
			return "#<syntax>";
		else
			return "#<syntax " + Name + ">";
	}
	
	/**
	 * Apply the macro directly (calls macroApply).
	 * 
	 * @param k What to do after applying the function.
	 * @param args Arguments to the function.
	 */
	public SchemeObject<?> apply(SchemeObject<?>... args) {
		SExpression[] sargs = new SExpression[args.length];
		for (int i = 0; i < args.length; i++) {
			verifyTypeOf(1, args[0], SExpression.class);
			sargs[i] = (SExpression) args[i];
		}
		
		return macroApply(sargs);
	}
	
	/**
	 * Special apply function for macros. Override this one instead of the other in gneral.
	 * @param args
	 * @return
	 */
	public abstract SchemeObject<?> macroApply(SExpression... args);
}
