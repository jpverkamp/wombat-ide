package wombat.scheme.library;

import wombat.scheme.*;
import wombat.scheme.library.base.*;

/**
 * Base library.
 */
public class Base {
	private Base() {}
	
	/**
	 * Load base methods.
	 */
	public static void load(Environment env) {
		EquivalencePredicates.load(env);
		Numbers.load(env);
		Booleans.load(env);
		PairsAndLists.load(env);
		Symbols.load(env);
		Characters.load(env);
		Strings.load(env);
		Vectors.load(env);
		Bytevectors.load(env);
		ControlFeatures.load(env);
		Exceptions.load(env);
		Eval.load(env);
		InputAndOutput.load(env);
	}
}
