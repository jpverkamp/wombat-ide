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
		
		env.defineMacro(new SchemeMacro("if") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				verifyListArity(args.length, 2, 3);
				
				// Tag that can contain the branches and will choose between them based on the top value.
				sexps.push(new IfTag(args[1], args.length == 2 ? SExpression.literal(SchemeVoid.singleton()) : args[2]));
				envs.push(env);
				
				// Then push on the conditional so it gets evaluated.
				sexps.push(args[0]);
				envs.push(env);
			}
		});
		
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
				
				verifyExactArity(args.length, 2);
				if (args[0].isLiteral())
					verifyTypeOf(1, args[0].getLiteral(), SchemeSymbol.class);
				else
					verifyTypeOf(1, args[0], SchemeSymbol.class);
				
				// Push on a tag that will do that actual setting.
				sexps.push(new DefineTag((SchemeSymbol) args[0].getLiteral()));
				envs.push(env);
				
				// Then push on the value to evaluate.
				sexps.push(args[1]);
				envs.push(env);
			}
		});
	}
	
	
}

class IfTag extends Tag {
	private static final long serialVersionUID = 3729440849677325667L;
	
	SExpression OnTrue;
	SExpression OnFalse;
	
	public IfTag(SExpression onTrue, SExpression onFalse) {
		OnTrue = onTrue;
		OnFalse = onFalse;
	}
	
	public void apply(
			Stack<SExpression> sexps,
			Stack<Environment> envs,
			Stack<SchemeObject<?>> values, 
			Environment env) {
		
		SchemeObject<?> cond = values.pop();
		if (cond instanceof SchemeBoolean && !((SchemeBoolean) cond).getValue())
			sexps.push(OnFalse);
		else
			sexps.push(OnTrue);
		envs.push(env);
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
