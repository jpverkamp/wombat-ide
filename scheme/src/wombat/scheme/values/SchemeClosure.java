package wombat.scheme.values;

import java.util.Arrays;
import java.util.Stack;

import wombat.scheme.Environment;
import wombat.scheme.SExpression;
import wombat.scheme.Tag;
import wombat.scheme.errors.SchemeSyntaxError;


/**
 * Represents a procedure created using lambda.
 */
public class SchemeClosure extends SchemeMacro {
	private static final long serialVersionUID = 4718435072962680718L;

	SchemeSymbol[] Params;
	SchemeSymbol Rest;
	Environment ClosureEnv;
	SExpression[] Bodies;
	
	/**
	 * Create a new closure with n parameters.
	 * @param params Parameters.
	 * @param env The environment the closure was created in.
	 * @param bodies The bodies of the closure.
	 */
	public SchemeClosure(SchemeSymbol[] params, Environment env, SExpression[] bodies) {
		this(params, null, env, bodies);
	}
	
	/**
	 * Create a new closure with only a rest parameter.
	 * @param rest The rest parameter (wraps parameters into a list)
	 * @param env The environment the closure was created in.
	 * @param bodies The bodies of the closure.
	 */
	public SchemeClosure(SchemeSymbol rest, Environment env, SExpression[] bodies) {
		this(null, rest, env, bodies);
	}

	/**
	 * Create a new closure with n parameters and a rest for the rest.
	 * @param params Parameters.
	 * @param rest The rest parameter (wraps parameters into a list)
	 * @param env The environment the closure was created in.
	 * @param bodies The bodies of the closure.
	 */
	public SchemeClosure(SchemeSymbol[] params, SchemeSymbol rest, Environment env, SExpression[] bodies) {
		Params = params;
		Rest = rest;
		ClosureEnv = env;
		Bodies = bodies;
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
	
	/**
	 * Special apply function for closures.
	 * 
	 * @param sexps The stack of s-expression to manipulate.
	 * @param envs The stack of environments (must have one per s-expression).
	 * @param values The stack of values already evaluated.
	 * @param env The current environment.
	 * @param args Any arguments to the macro (as s-expressions).
	 */
	public void closureApply(
			Stack<SExpression> sexps,
			Stack<Environment> envs,
			Stack<SchemeObject<?>> values, 
			Environment env,
			SchemeObject<?>... args) {
		
		if (Rest != null)
			if (Params != null) // otherwise accepts any number of arguments
				verifyMinimumArity(args.length, Params.length);
		else
			verifyExactArity(args.length, Params.length);
		
		System.err.println("closure:");
		if (Params != null) System.err.println("\tparams: " + Arrays.toString(Params));
		if (Rest != null) System.err.println("\trest: " + Rest);
		System.err.println("\tvalues: " + Arrays.toString(args));
		System.err.println("\told env: " + ClosureEnv);
		
		// Build the new environment
		Environment newEnv = ClosureEnv.extend();
		if (Params != null) {
			for (int i = 0; i < Params.length; i++)
				newEnv.define(Params[i], args[i]);
		}
		if (Rest != null) {
			int restStart = (Params == null ? 0 : Params.length);
			SchemeObject<?>[] ls = new SchemeObject<?>[Params.length - restStart];
			for (int i = 0; i < ls.length; i++)
				ls[i] = args[i - restStart];
			newEnv.define(Rest, SchemePair.fromList(ls));
		}
		
		System.err.println("\tnew env: " + newEnv);
		
		// Push each body with the new environment
		// Also push something to eat the value from each except the last
		for (int i = Bodies.length - 1; i >= 0; i--) {
			sexps.push(Bodies[i]);
			envs.push(newEnv);
			if (i != 0) {
				sexps.push(new ValueEaterTag());
				envs.push(newEnv);
			}
		}
	}

	/**
	 * Apply the macro (or rather, don't)
	 */
	public void macroApply(Stack<SExpression> sexps, Stack<Environment> envs, Stack<SchemeObject<?>> values, Environment env, SExpression... args) {
		throw new SchemeSyntaxError(this, "Closures should not be called as macros");
		
	}
}

/**
 * Class to eat up (ignore) a single returned value.
 */
class ValueEaterTag extends Tag {
	private static final long serialVersionUID = 8883620994897093516L;

	@Override
	public void apply(Stack<SExpression> sexps, Stack<Environment> envs, Stack<SchemeObject<?>> values, Environment env) {
		values.pop();
	}
}