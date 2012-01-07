package wombat.scheme.library.base;

import java.util.*;

import wombat.scheme.*;
import wombat.scheme.errors.*;
import wombat.scheme.values.*;

public class BindingConstructs {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		// Set a variable. It must have been previously defined.
		env.defineMacro(new SchemeMacro("set!") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				verifyExactArity(args.length, 2);
				if (args[0].isLiteral())
					verifyTypeOf(1, args[0].getLiteral(), SchemeSymbol.class);
				else
					verifyTypeOf(1, args[0], SchemeSymbol.class);
				
				// Push on a tag that will do that actual setting.
				sexps.push(new BindingTag((SchemeSymbol) args[0].getLiteral(), false));
				envs.push(env);
				
				// Then push on the value to evaluate.
				sexps.push(args[1]);
				envs.push(env);
			}
		});
		
		// Define a new variable in the current scope.
		env.defineMacro(new SchemeMacro("define") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				verifyMinimumArity(args.length, 2);
				
				// If the first thing is literal, make sure it's a symbol, then bind it.
				if (args[0].isLiteral()) {
					verifyExactArity(args.length, 2);
					verifyTypeOf(1, args[0].getLiteral(), SchemeSymbol.class);
					
					// Push on a tag that will do that actual setting.
					sexps.push(new BindingTag((SchemeSymbol) args[0].getLiteral(), true));
					envs.push(env);
					
					// Then push on the value to evaluate.
					sexps.push(args[1]);
					envs.push(env);
				}
				
				// Otherwise, it's the short function define form
				// Rewrite: (define (name ...) ...) => (define name (lambda (...) ...))
				else {
					// Make sure they're all symbols and that if there is a dot, it's the second last argument.
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
					for (int i = 1; i < size; i++)
						for (int j = i + 1; j < size; j++)
							if (args[0].getList().get(i).getLiteral().equals(args[0].getList().get(j).getLiteral()))
								throw new SchemeSyntaxError(this, "Duplicate parameter '" + args[0].getList().get(i).getLiteral().display() + "' at " + (i + 1) + " and " + (j + 1));
					
					// Get the name
					SchemeSymbol name = (SchemeSymbol) args[0].getList().get(0).getLiteral();
					
					// Build the body list.
					SExpression[] bodies = new SExpression[args.length - 1];
					for (int i = 0; i < bodies.length; i++)
						bodies[i] = args[i + 1];
					
					// Now, build the new closure and then store them

					// (define (name) bodies ...) => (define name (lambda () bodies ...)
					if (args[0].getList().size() == 1) {
						SchemeClosure toDefine = new SchemeClosure(new SchemeSymbol[0], env, bodies);
						toDefine.setName(name.getValue());
						env.define(name, toDefine);
					}
					
					// (define (name . x) bodies ...) => (define name (lambda x bodies ...))
					else if (args[0].getList().size() == 3 && ((SchemeSymbol) args[0].getList().get(1).getLiteral()).isDot()) {
						SchemeSymbol rest = ((SchemeSymbol) args[0].getList().get(size - 1).getLiteral());
						
						SchemeClosure toDefine = new SchemeClosure(rest, env, bodies);
						toDefine.setName(name.getValue());
						env.define(name, toDefine);
					}
					
					// (define (name x . y) bodies ...) => (define name (lambda (x . y) bodies ...))
					else if (((SchemeSymbol) args[0].getList().get(size - 2).getLiteral()).isDot()) {
						SchemeSymbol[] params = new SchemeSymbol[size - 2];
						for (int i = 0; i < params.length; i++)
							params[i] = ((SchemeSymbol) args[0].getList().get(i + 1).getLiteral());
						SchemeSymbol rest = ((SchemeSymbol) args[0].getList().get(size - 1).getLiteral());
						
						SchemeClosure toDefine = new SchemeClosure(params, rest, env, bodies);
						toDefine.setName(name.getValue());
						env.define(name, toDefine);
					}
					
					// (define (name x y z) bodies ...) => (define name (lambda (x y z) bodies ...))
					else {
						SchemeSymbol[] params = new SchemeSymbol[size - 1];
						for (int i = 0; i < params.length; i++)
							params[i] = ((SchemeSymbol) args[0].getList().get(i + 1).getLiteral());
						
						SchemeClosure toDefine = new SchemeClosure(params, env, bodies);
						toDefine.setName(name.getValue());
						env.define(name, toDefine);
					}
					
					// In any case, return void.
					values.push(SchemeVoid.singleton());
				}
			}
		});
		
		// Two possibilities:
		// - Simplest form of variable binding. Bind a series of variables that cannot refer to each other.
		// - A named let which essentially binds a function to the let definition.
		env.defineMacro(new SchemeMacro("let") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

				verifyMinimumArity(args.length, 2);
				
				SchemeSymbol name; // will be null if not a named let
				List<SExpression> bindings;				
				int firstBody;
				
				// Check if we are dealing with a normal let or a named let.
				
				// Named let
				if (args[0].isLiteral()) {
					verifyTypeOf(1, args[0].getLiteral(), SchemeSymbol.class);
					name = (SchemeSymbol) args[0].getLiteral();
					
					if (!args[1].isList())
						verifyTypeOf(2, args[1], SchemePair.class);
					bindings = args[1].getList();
					
					firstBody = 2;
				}
				
				// Regular let
				else {
					name = null;
					
					if (!args[0].isList())
						verifyTypeOf(1, args[0], SchemePair.class);
					bindings = args[0].getList();
					
					firstBody = 1;
				}
				
				// Verify that all of the bindings are pairs of names and expressions and that there aren't any duplicates
				for (int i = 0; i < bindings.size(); i++) {
					if (bindings.get(i).isLiteral() || bindings.get(i).getList().size() != 2)
						throw new SchemeSyntaxError(this, "Invalid binding at " + bindings.get(i).getLiteral().display() + ", expected variable name and expression.");
					
					if (bindings.get(i).getList().get(0).isLiteral())
						verifyTypeOf(firstBody - 1, bindings.get(i).getList().get(0).getLiteral(), SchemeSymbol.class);
					else
						verifyTypeOf(firstBody - 1, bindings.get(i).getList().get(0), SchemeSymbol.class);
					
					if (((SchemeSymbol) bindings.get(i).getList().get(0).getLiteral()).isDot())
						throw new SchemeSyntaxError(this, "A dot is not a valid name for a binding.");
					
					for (int j = 0; j < i; j++)
						if (bindings.get(i).getList().get(0).getLiteral().equals(bindings.get(j).getList().get(0).getLiteral()))
							throw new SchemeSyntaxError(this, "Duplicate identifier '" + bindings.get(i).getList().get(0).getLiteral().display() + "'");
				}
				
				// Create the new environment that all of these bindings are going to get pushed into.
				Environment newEnv = env.extend();
				
				// Push the bodies that are going to get evaluated in that new environment.
				// Push them first so they get evaluated last.
				// Each except for the last gets a value eater.
				sexps.push(args[args.length - 1]);
				envs.push(newEnv);
				
				for (int i = args.length - 2; i >= firstBody; i--) { 
					sexps.push(new ValueEaterTag());
					envs.push(newEnv);
					
					sexps.push(args[i]);
					envs.push(newEnv);
				}

				// First a binder for each pair (to go after all the bindings are evaluated)
				// Bindings are all in the new environment
				for (SExpression bind : bindings) {
					sexps.push(new BindingTag((SchemeSymbol) bind.getList().get(0).getLiteral(), true));
					envs.push(newEnv);
				}
				
				// Now evaluate all the bindings first
				// Evaluations are all in the original environment
				for (SExpression bind : bindings) {
					sexps.push(bind.getList().get(1));
					envs.push(env);
				}
			}
		});
		
		env.defineMacro(new SchemeMacro("let*") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("letrec") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("letrec*") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("let-values") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("do") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
	}
}

/**
 * Used to set or define variables.
 */
class BindingTag extends Tag {
	private static final long serialVersionUID = -6219294329111768317L;
	
	SchemeSymbol Name;
	boolean IsDefine;
	
	/**
	 * Create a new set tag. 
	 * @param name The name of the variable that will be set.
	 * @param isDefine If the variable should be defined (it will be set if not).
	 */
	public BindingTag(SchemeSymbol name, boolean isDefine) {
		Name = name;
		IsDefine = isDefine;
	}
	
	/**
	 * Apply the tag.
	 */
	public void apply(
			Stack<SExpression> sexps,
			Stack<Environment> envs,
			Stack<SchemeObject<?>> values, 
			Environment env) {
		
		SchemeObject<?> toSet = values.pop();
		if (toSet instanceof SchemeProcedure)
			((SchemeProcedure) toSet).setName(Name.getValue());
		
		if (IsDefine)
			env.define(Name, toSet);
		else
			env.set(Name, toSet);
		values.push(SchemeVoid.singleton());
	}
}

/**
 * Class to eat up (ignore) a single returned value.
 */
class ValueEaterTag extends Tag {
	private static final long serialVersionUID = 8883620994897093516L;

	@Override
	public void apply(Stack<SExpression> sexps, Stack<Environment> envs, Stack<SchemeObject<?>> values, Environment env) {
		values.pop();
	}
}
