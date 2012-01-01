package wombat.scheme;

import java.util.Arrays;
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
			// DEBUG
			System.out.println(" Sexps: " + Arrays.toString(sexps.toArray()));
			System.out.println("  Envs: " + Arrays.toString(envs.toArray()));
			System.out.println("Values: " + Arrays.toString(values.toArray()));
			System.out.println();
			
			
			sexp = sexps.pop();
			env = envs.pop();
		
			// Deal with tags.
			if (sexp instanceof Tag) {
				// Check if we're going to be doing a macro or a procedure
				if (sexp instanceof ProcedureOrMacroTag) {
					// Get the procedure and the rest of the arguments
					SchemeObject<?> procOrMacro = values.pop();
					SExpression rands = sexps.pop();
					
					// Deal with macros.
					if (procOrMacro instanceof SchemeMacro) {
						// Push the arguments back on directly, then the macro
						for (int i = rands.getList().size() - 1; i >= 1; i--)
							values.push(rands.getList().get(i));
						values.push(procOrMacro);
					}
					
					// Deal with procedures (has to be second because Macro is a subclass)
					else if (procOrMacro instanceof SchemeProcedure) {
						// Push the procedure, then the arguments to evaluate.
						sexps.push(SExpression.literal(procOrMacro));
						envs.push(env);
						for (int i = 1; i < rands.getList().size(); i++) {
							sexps.push(rands.getList().get(i));
							envs.push(env);
						}						
					}
					
					// Otherwise, this isn't applyable
					else 
						throw new SchemeRuntimeError(procOrMacro, procOrMacro.display() + " is not a procedure");
					
					// Either way, push the thing to run back on.
					
				}
				
				// Application.
				else if (sexp instanceof ApplicationTag) {
					// Get the rator and rands off the stack.
					SchemeObject<?> rator = values.pop();
					SchemeObject<?>[] rands = new SchemeObject<?>[((ApplicationTag) sexp).Args];
					for (int i = 0; i < rands.length; i++)
						rands[i] = values.pop();
					
					// Evaluate macros (might return either an s-expression or a literal
					if (rator instanceof SchemeMacro) {
						SchemeObject<?> result = ((SchemeProcedure) rator).apply(rands);
						if (result instanceof SExpression) {
							sexps.push((SExpression) result);
							envs.push(env);
						} else {
							values.push(result);
						}
					}
					
					// Evaluate procedures / macros (they're both really the same thing now)
					else if (rator instanceof SchemeProcedure)
						values.push(((SchemeProcedure) rator).apply(rands)); 
					
					// Otherwise, it's not a procedure or a macro. EXPLODE!
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

				// Push this s-expression back, the next tag will pull it back off and do what it needs to
				sexps.push(sexp);
				
				// Push the choice we'll have to make if we're going to do a macro or a procedure.
				sexps.push(new ProcedureOrMacroTag());
				envs.push(env);
				
				// Now push the rator/rand
				sexps.push(sexp.getList().get(0));
				envs.push(env);
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

/**
 * Tag to check if we're dealing with a procedure or a macro.
 */
class ProcedureOrMacroTag extends Tag {
	private static final long serialVersionUID = -4881239183721789528L;
}
