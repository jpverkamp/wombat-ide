package wombat.scheme;

import wombat.scheme.values.*;

/**
 * Evaluate s-expressions.
 */
public class Evaluator {
	/** 
	 * Hide constructor. 
	 */
	private Evaluator() {}
	
	/**
	 * Evaluate an s-expression.
	 * @param sexp The expression to evaluate.
	 * @return The resulting scheme object.
	 */
	public static SchemeObject<?> evaluate(SExpression sexp) {
		return new SchemeVoid();
	}
}
