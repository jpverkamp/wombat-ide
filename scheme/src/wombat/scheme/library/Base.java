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
		// From Section 4.1 of R7RS (draft 5)
		CoreExpressions.load(env);
		
		// From Section 6 of R7RS (draft 5)
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
