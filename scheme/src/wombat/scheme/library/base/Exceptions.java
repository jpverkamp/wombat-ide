package wombat.scheme.library.base;

import wombat.scheme.Environment;
import wombat.scheme.errors.*;
import wombat.scheme.values.*;

public class Exceptions {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("with-exception-handler") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("raise") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				throw new SchemeRuntimeError(args[0], "raise called");
			}
		});
		
		env.defineProcedure(new SchemeProcedure("raise-continuable") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("error") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeString.class);
				
				if (args.length == 1) {
					throw new SchemeRuntimeError(this, ((SchemeString) args[0]).getValue());
				} else {
					SchemeObject<?>[] irritants = new SchemeObject<?>[args.length - 1];
					for (int i = 0; i < irritants.length; i++)
						irritants[i] = args[i + 1];					
				
					throw new SchemeRuntimeError(this, ((SchemeString) args[0]).getValue(), irritants);
				}
			}
		});
		
		env.defineProcedure(new SchemeProcedure("error-object?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("error-object-message") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("error-object-irritants") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
	}
}
