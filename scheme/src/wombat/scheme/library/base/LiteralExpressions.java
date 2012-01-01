package wombat.scheme.library.base;

import wombat.scheme.*;
import wombat.scheme.values.*;

public class LiteralExpressions {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineMacro(new SchemeMacro("quote") {
			@Override
			public SchemeObject<?> macroApply(SExpression... args) {
				verifyExactArity(args.length, 1);
				return args[0].deSExpression();
			}
		});
	}
	
	
}
