package wombat.scheme.library.base;

import wombat.scheme.Environment;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.SchemeObject;
import wombat.scheme.values.SchemeProcedure;
import wombat.scheme.values.numeric.SchemeInteger;
import wombat.scheme.values.numeric.SchemeNumber;

public class Numbers {
	private Numbers() {}
	
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
	}
}
