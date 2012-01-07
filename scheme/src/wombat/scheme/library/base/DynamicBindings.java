package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.Environment;
import wombat.scheme.SExpression;
import wombat.scheme.values.SchemeMacro;
import wombat.scheme.values.SchemeObject;

public class DynamicBindings {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineMacro(new SchemeMacro("make-parameter") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
		env.defineMacro(new SchemeMacro("parameterize") {
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
