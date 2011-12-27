package wombat.scheme.library;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.*;
import wombat.scheme.values.numeric.*;

/**
 * Base library.
 */
public class Base {
	private Base() {}
	
	/**
	 * Load base methods.
	 */
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("+") {
			private static final long serialVersionUID = 7905004423840662886L;

			public SchemeObject<?> apply(SchemeObject<?>... args) {
				SchemeNumber<?> result = new SchemeInteger("0");
				
				for (SchemeObject<?> arg : args)
					if (arg instanceof SchemeNumber<?>)
						result = result.add((SchemeNumber<?>) arg);
					else 
						throw new SchemeRuntimeError(this, arg.write() + " is not a number");
				
				return result;
			}
		});
		
		env.defineProcedure(new SchemeProcedure("display") {
			private static final long serialVersionUID = -1287690738553847651L;

			public SchemeObject<?> apply(SchemeObject<?>... args) { 
				verifyExactArity(args.length, 1);
				
				return new SchemeString(args[0].display());
			}
		});
		
		env.defineProcedure(new SchemeProcedure("write") {
			private static final long serialVersionUID = -75980137762828144L;

			public SchemeObject<?> apply(SchemeObject<?>... args) { 
				verifyExactArity(args.length, 1);
				
				return new SchemeString(args[0].write());
			}
		});
	}
}
