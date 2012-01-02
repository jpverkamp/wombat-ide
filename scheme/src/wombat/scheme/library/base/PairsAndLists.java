package wombat.scheme.library.base;

import wombat.scheme.Environment;
import wombat.scheme.values.*;

public class PairsAndLists {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("cons") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 2);
				return new SchemePair(args[0], args[1]);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("car") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemePair.class);
				return ((SchemePair) args[0]).getCar();
			}
		});
		
		env.defineProcedure(new SchemeProcedure("cdr") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemePair.class);
				return ((SchemePair) args[0]).getCdr();
			}
		});
	}
}
