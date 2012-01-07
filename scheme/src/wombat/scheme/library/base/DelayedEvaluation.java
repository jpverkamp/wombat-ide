package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.Environment;
import wombat.scheme.SExpression;
import wombat.scheme.values.SchemeMacro;
import wombat.scheme.values.SchemeObject;

public class DelayedEvaluation {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineMacro(new SchemeMacro("delay") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("lazy") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("force") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("eager") {
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
