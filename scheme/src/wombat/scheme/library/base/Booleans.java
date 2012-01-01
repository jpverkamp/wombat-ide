package wombat.scheme.library.base;

import wombat.scheme.*;
import wombat.scheme.values.*;

public class Booleans {
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
	}
}
