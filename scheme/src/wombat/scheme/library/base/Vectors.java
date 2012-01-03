package wombat.scheme.library.base;

import java.util.Stack;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.*;

public class Vectors {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("vector") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				return new SchemeVector(args);
			}
		});
		
		env.defineMacro(new SchemeMacro("literal-vector") {
			public void macroApply(
					final Stack<SExpression> sexps,
					final Stack<Environment> envs, 
					final Stack<SchemeObject<?>> values,
					final Environment env, 
					SExpression... args) {
				
				verifyExactArity(args.length, 1);
				if (args[0].isList())
					values.push(new SchemeVector(((SchemePair) args[0].deSExpression()).toList()));
				else
					throw new SchemeRuntimeError(this, args[0].getLiteral() + " is not a valid vector");				
			}
		});
	}
}
