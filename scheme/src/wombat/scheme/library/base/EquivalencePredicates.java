package wombat.scheme.library.base;

import wombat.scheme.*;
import wombat.scheme.values.*;
import wombat.scheme.values.numeric.*;

public class EquivalencePredicates {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		final SchemeProcedure eqv = new SchemeProcedure("eqv?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 2);
				
				if (args[0] instanceof SchemeBoolean && args[1] instanceof SchemeBoolean)
					return new SchemeBoolean(args[0].getValue().equals(args[1].getValue()));

				if (args[0] instanceof SchemeSymbol && args[1] instanceof SchemeSymbol)
					return new SchemeBoolean(args[0].getValue().equals(args[1].getValue()));
				
				if (args[0] instanceof SchemeNumber<?> && args[1] instanceof SchemeNumber<?>)
					return new SchemeBoolean(args[0].getValue().equals(args[1].getValue()));
				
				if (args[0] instanceof SchemeCharacter && args[1] instanceof SchemeCharacter)
					return new SchemeBoolean(args[0].getValue().equals(args[1].getValue()));
				
				if (args[0] instanceof SchemeEmptyList && args[1] instanceof SchemeEmptyList)
					return new SchemeBoolean(true);
				
				if (args[0] == args[1] || args[0].getValue() == args[1].getValue())
					return new SchemeBoolean(true);

				return new SchemeBoolean(false);
			}
		};
		env.defineProcedure(eqv);
		
		final SchemeProcedure eq = new SchemeProcedure("eq?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 2);
				
				return eqv.apply(args);
			}
		};
		env.defineProcedure(eq);
		
		final SchemeProcedure equal = new SchemeProcedure("equal?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 2);
				
				return new SchemeBoolean(args[0].equals(args[1]));
			}
		};
		env.defineProcedure(equal);
	}
}
