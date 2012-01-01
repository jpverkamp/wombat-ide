package wombat.scheme.library.base;

import wombat.scheme.Environment;
import wombat.scheme.values.*;
import wombat.scheme.values.numeric.*;

public class Numbers {
	private Numbers() {}
	
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("+") {
			@SuppressWarnings("unchecked")
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(1, args[i], (Class<? extends SchemeObject<?>>) SchemeNumber.class);
				
				SchemeNumber<?> result = new SchemeInteger("0");
				for (SchemeObject<?> arg : args)
					result = result.add((SchemeNumber<?>) arg);
				
				return result;
			}
		});
	}
}
