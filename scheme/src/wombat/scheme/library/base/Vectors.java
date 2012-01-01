package wombat.scheme.library.base;

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
			public SchemeObject<?> macroApply(SExpression... args) {
				verifyExactArity(args.length, 1);
				if (args[0].isList())
					return new SchemeVector(((SchemePair) args[0].deSExpression()).toList());
				else
					throw new SchemeRuntimeError(this, args[0].getLiteral() + " is not a valid vector");
			}
		});
	}
}
