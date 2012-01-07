package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.Environment;
import wombat.scheme.SExpression;
import wombat.scheme.values.SchemeMacro;
import wombat.scheme.values.SchemeObject;

public class Sequencing {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		// Sequencing
		env.defineMacro(new SchemeMacro("begin") {
			public void macroApply(
					final Stack<SExpression> sexps, 
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env,
					SExpression... args
					) {

			}
		});
		
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
	}
}
