package wombat.scheme;

import java.util.Stack;

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
		Stack<SExpression> sexps = new Stack<SExpression>();
		Stack<Environment> envs = new Stack<Environment>();
		Stack<SchemeObject<?>> values = new Stack<SchemeObject<?>>();
		
		sexps.push(sexp);
		envs.push(env);
		
		// Keep going until we run out of expressions to evaluate.
		while (!(sexps.isEmpty())) {
			sexp = sexps.pop();
			env = envs.pop();
		
			// Deal with tags.
			if (sexp instanceof Tag) {
				// Application.
				if (sexp instanceof ApplicationTag) {
					// Get the rator and rands off the stack.
					SchemeObject<?> rator = values.pop();
					SchemeObject<?>[] rands = new SchemeObject<?>[((ApplicationTag) sexp).Args];
					for (int i = 0; i < rands.length; i++)
						rands[i] = values.pop();
					
					// Evaluate procedures
					if (rator instanceof SchemeProcedure)
						values.push(((SchemeProcedure) rator).apply(rands));
						
					// TODO: Evaluate macros
					//else if 
					
					// Otherwise, it's not a procedure. EXPLODE!
					else
						throw new SchemeRuntimeError(rator, rator.display() + " is not a procedure");
				}
			}
			
			// Deal with literals
			else if (sexp.isLiteral()) {
				// Look up literals in the environment
				if (sexp.LiteralValue instanceof SchemeSymbol)
					values.push(env.get((SchemeSymbol) sexp.getLiteral()));
				
				// Everything else, push directly
				else
					values.push(sexp.getLiteral());
			}
			
			// Apply procedures.
			else {
				// Sanity check for empty lists.
				if (sexp.getList().size() == 0)
					throw new SchemeRuntimeError(sexp, "Missing procedure");
				
				// First, push an application tag
				// This will tell us when to actually do the evaluation and how many arguments it wants
				sexps.push(new ApplicationTag(sexp.getList().size() - 1));
				envs.push(env);
				
				// Push the rator then the rands to evaluate them (each with it's own environment)
				for (SExpression each : sexp.getList()) {
					sexps.push(each);
					envs.push(env);
				}
			}
		}
		
		// At that point, return the values.
		return values.pop();
	}
}

/**
 * Tags that will be inserted into the s-expression stack to signify interesting things to do.
 */
abstract class Tag extends SExpression {
	private static final long serialVersionUID = -4350298610979134739L;
} 

/**
 * Apply something with the given number of arguments.
 */
class ApplicationTag extends Tag {
	private static final long serialVersionUID = 7996280213010060432L;  

	int Args;
	
	/**
	 * Create an application tag.
	 * @param args The number of arguments the application expects.
	 */
	public ApplicationTag(int args) {
		Args = args;
	}
}
