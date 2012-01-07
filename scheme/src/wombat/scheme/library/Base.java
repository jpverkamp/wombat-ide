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
		BindingConstructs.load(env);
		Bytevectors.load(env);
		Characters.load(env);
		Conditionals.load(env);
		ControlFeatures.load(env);
		DelayedEvaluation.load(env);
		DynamicBindings.load(env);
		EquivalencePredicates.load(env);
		Eval.load(env);
		Exceptions.load(env);
		InputAndOutput.load(env);
		Numbers.load(env);
		PairsAndLists.load(env);
		Sequencing.load(env);
		Strings.load(env);
		Symbols.load(env);
		Vectors.load(env);
	}
}
