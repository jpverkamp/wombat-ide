package wombat.scheme;

import java.util.Stack;

import wombat.scheme.values.SchemeObject;

/**
 * Tags that will be inserted into the s-expression stack to signify interesting things to do.
 */
public abstract class Tag extends SExpression {
	private static final long serialVersionUID = -4350298610979134739L;
	
	/**
	 * Apply the tag.
	 * 
	 * @param sexps The stack of s-expression to manipulate.
	 * @param envs The stack of environments (must have one per s-expression).
	 * @param values The stack of values already evaluated.
	 * @param env The current environment.
	 */
	public abstract void apply(
			Stack<SExpression> sexps, 
			Stack<Environment> envs, 
			Stack<SchemeObject<?>> values,
			Environment env);
} 
