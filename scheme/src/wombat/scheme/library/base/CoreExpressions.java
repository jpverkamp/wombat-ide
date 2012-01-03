package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.errors.SchemeSyntaxError;
import wombat.scheme.values.*;

public class CoreExpressions {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineMacro(new SchemeMacro("quote") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

				verifyExactArity(args.length, 1);
				values.push(args[0].deSExpression());
			}
		});
		
		env.defineMacro(new SchemeMacro("lambda") {
			public void macroApply(
					final Stack<SExpression> sexps,
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args) {
				
				verifyMinimumArity(args.length, 2);

				// Get the bodies.
				final SExpression[] bodies = new SExpression[args.length - 1];
				for (int i = 0; i < bodies.length; i++)
					bodies[i] = args[i + 1];
				
				// Get the parameters and build the closure
				
				// Only a rest parameter
				if (args[0].isLiteral()) {
					if (args[0].getLiteral() instanceof SchemeSymbol) {
						values.push(new SchemeClosure((SchemeSymbol) args[0].getLiteral(), env, bodies).at(Line, Column));
					} else {
						throw new SchemeRuntimeError(this, args[0].getLiteral().display() + " is not a valid parameter");
					}
				}
				
				// Otherwise a list.
				else {
					// Make sure that everything is a symbol (and not a dot, except for the second last)
					int size = args[0].getList().size();
					for (int i = 0; i < size; i++) {
						if (args[0].getList().get(i).isLiteral())
							verifyTypeOf(i + 1, args[0].getList().get(i).getLiteral(), SchemeSymbol.class);
						else
							verifyTypeOf(i + 1, args[0].getList().get(i), SchemeSymbol.class);
						
						if (i != size - 2 && ((SchemeSymbol) args[0].getList().get(i).getLiteral()).isDot())
							throw new SchemeSyntaxError(this, "Invalid dot at argument " + (i + 1));
					}
					
					// Check for duplicates.
					for (int i = 0; i < size; i++)
						for (int j = i + 1; j < size; j++)
							if (args[0].getList().get(i).getLiteral().equals(args[0].getList().get(j).getLiteral()))
								throw new SchemeSyntaxError(this, "Duplicate parameter '" + args[0].getList().get(i).getLiteral().display() + "' at " + (i + 1) + " and " + (j + 1));					
					
					// Check for a rest parameter.
					if (size > 1 && ((SchemeSymbol) args[0].getList().get(size - 2).getLiteral()).isDot()) {
						SchemeSymbol[] params = new SchemeSymbol[size - 2];
						for (int i = 0; i < size - 2; i++)
							params[i] = (SchemeSymbol) args[0].getList().get(i).getLiteral();
						SchemeSymbol rest = (SchemeSymbol) args[0].getList().get(size - 1).getLiteral();
						
						values.push(new SchemeClosure(params, rest, env, bodies).at(Line, Column));
					}
					
					// No rest parameter.
					else {
						SchemeSymbol[] params = new SchemeSymbol[size];
						for (int i = 0; i < size; i++)
							params[i] = (SchemeSymbol) args[0].getList().get(i).getLiteral();
						
						values.push(new SchemeClosure(params, env, bodies).at(Line, Column));
					}
				}
			}
		});
		
		// if
		// set!
		// define
	}
	
	
}
