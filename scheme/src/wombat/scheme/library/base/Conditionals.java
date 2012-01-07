package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.*;
import wombat.scheme.values.*;

public class Conditionals {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		// (not x) returns #t if x == #f, #f otherwise
		env.defineProcedure(new SchemeProcedure("not") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				
				return new SchemeBoolean(
						args[0] instanceof SchemeBoolean 
						&& !((SchemeBoolean) args[0]).getValue()
					);
			}
		});
		
		// (boolean? x) returns #t if x is a boolean and false otherwise
		env.defineProcedure(new SchemeProcedure("boolean?") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				
				return new SchemeBoolean(args[0] instanceof SchemeBoolean);
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
		
		env.defineMacro(new SchemeMacro("cond") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
			}
		});
		
		env.defineMacro(new SchemeMacro("case") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
			}
		});
		
		env.defineMacro(new SchemeMacro("and") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
			}
		});
		
		env.defineMacro(new SchemeMacro("or") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
			}
		});
		
		env.defineMacro(new SchemeMacro("when") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
			}
		});
		
		env.defineMacro(new SchemeMacro("unless") {
			public void macroApply(
					Stack<SExpression> sexps,
					Stack<Environment> envs,
					Stack<SchemeObject<?>> values,
					Environment env, SExpression... args) {
				
				
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
