package wombat.scheme.values;

import java.util.*;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeSyntaxError;

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
		throw new SchemeSyntaxError(this, "Macros should not be called as procedures");
	}
	
	/**
	 * Special apply function for macros. Override this one instead of the other in gneral.
	 * @param sexps The stack of s-expression to manipulate.
	 * @param envs The stack of environments (must have one per s-expression).
	 * @param values The stack of values already evaluated.
	 * @param env The current environment.
	 * @param args Any arguments to the macro (as s-expressions).
	 */
	public abstract void macroApply(
			final Stack<SExpression> sexps, 
			final Stack<Environment> envs, 
			final Stack<SchemeObject<?>> values,
			final Environment env,
			SExpression... args);
}
