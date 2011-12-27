package wombat.scheme;

import wombat.scheme.errors.*;
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
		if (sexp.isLiteral() && sexp.getLiteral() instanceof SchemeSymbol)
			return env.get((SchemeSymbol) sexp.getLiteral());
		else if (sexp.isLiteral())
			return sexp.getLiteral();
		else {
			if (sexp.getList().size() == 0)
				throw new SchemeRuntimeError(sexp, "Missing procedure");
			
			SchemeObject<?> rator = evaluate(sexp.getList().get(0), env);
			
			if (rator instanceof SchemeProcedure) {
				SchemeObject<?>[] rands = new SchemeObject<?>[sexp.getList().size() - 1];
				for (int i = 1; i < sexp.getList().size(); i++)
					rands[i - 1] = evaluate(sexp.getList().get(i), env);

				return ((SchemeProcedure) rator).apply(rands);
			} else
				throw new SchemeRuntimeError(rator, "Unable to apply non-procedure " + rator.write());
		}
	}
}
