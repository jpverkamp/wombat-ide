package wombat.scheme;

import wombat.scheme.continuations.*;
import wombat.scheme.errors.*;
import wombat.scheme.values.*;

/**
 * Evaluate s-expressions.
 */
public class ContinuationEvaluator {
	/** 
	 * Hide constructor. 
	 */
	private ContinuationEvaluator() {}
	
	/**
	 * Evaluate an s-expression.
	 * @param sexp The expression to evaluate.
	 * @param env The environment to evaluate the expression in.
	 * @return The resulting scheme object.
	 */
	public static SchemeObject<?> evaluate(SExpression sexp, Environment env) {
		// Phase 1: Syntax expansion
		// TODO
		
		// Phase 2: Build the initial continuation
		Continuation k = new ReturnContination();
		
		// If we have a symbol, we're going to look it up in the environment.
		if (sexp.isLiteral() && sexp.getLiteral() instanceof SchemeSymbol) {
			k = new LiteralContinuation(k, env.get((SchemeSymbol) sexp.getLiteral()));
		} 
			
		// All other literals, just return them directly.
		else if (sexp.isLiteral()) {
			k = new LiteralContinuation(k, sexp.getLiteral());
		}
			
		// Otherwise, an application (macros are gone at this point)
		else {
			// Verify that it's not an empty list.
			if (sexp.getList().size() == 0)
				throw new SchemeRuntimeError(sexp, k, "Missing procedure");
				
			// Start the continuation.
			k = new ApplicationContinuation(k, (SExpression[]) sexp.getList().toArray(), env);
		}
		
		// Phase 3: Evaluate the continuations and return the result.
		SchemeObject<?> next;
		while (true) {
			next = k.apply();
			if (k instanceof Continuation)
				k = (Continuation) next;
			else
				return next;
		}
	}
}