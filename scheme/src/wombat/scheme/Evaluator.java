package wombat.scheme;

import wombat.scheme.errors.SchemeRuntimeError;
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
	 * @param env The environment to evaluate the expression in.
	 * @return The resulting scheme object.
	 */
	public static SchemeObject<?> evaluate(SExpression sexp, Environment env) {
		if (sexp.isLiteral())
			return sexp.getLiteral();
		else 
			throw new SchemeRuntimeError(sexp, "Not implemented");
	}
}
