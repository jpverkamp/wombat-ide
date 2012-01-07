package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.Environment;
import wombat.scheme.SExpression;
import wombat.scheme.Tag;
import wombat.scheme.errors.SchemeSyntaxError;
import wombat.scheme.values.SchemeClosure;
import wombat.scheme.values.SchemeMacro;
import wombat.scheme.values.SchemeObject;
import wombat.scheme.values.SchemeProcedure;
import wombat.scheme.values.SchemeSymbol;
import wombat.scheme.values.SchemeVoid;

public class BindingConstructs {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
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
				sexps.push(new SetBangTag((SchemeSymbol) args[0].getLiteral()));
				envs.push(env);
				
				// Then push on the value to evaluate.
				sexps.push(args[1]);
				envs.push(env);
			}
		});
		
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
					sexps.push(new DefineTag((SchemeSymbol) args[0].getLiteral()));
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
		
		env.defineMacro(new SchemeMacro("let") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

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

class SetBangTag extends Tag {
	private static final long serialVersionUID = -6219294329111768317L;
	
	SchemeSymbol Name;
	
	public SetBangTag(SchemeSymbol name) {
		Name = name;
	}
	
	public void apply(
			Stack<SExpression> sexps,
			Stack<Environment> envs,
			Stack<SchemeObject<?>> values, 
			Environment env) {
		
		SchemeObject<?> toSet = values.pop();
		if (toSet instanceof SchemeProcedure)
			((SchemeProcedure) toSet).setName(Name.getValue());
		
		env.set(Name, toSet);
		values.push(SchemeVoid.singleton());
	}
}

class DefineTag extends Tag {
	private static final long serialVersionUID = 2046247061891921114L;
	
	SchemeSymbol Name;
	
	public DefineTag(SchemeSymbol name) {
		Name = name;
	}
	
	public void apply(
			Stack<SExpression> sexps,
			Stack<Environment> envs,
			Stack<SchemeObject<?>> values, 
			Environment env) {
		
		SchemeObject<?> toSet = values.pop();
		if (toSet instanceof SchemeProcedure)
			((SchemeProcedure) toSet).setName(Name.getValue());
		
		env.define(Name, toSet);
		values.push(SchemeVoid.singleton());
	}
}
