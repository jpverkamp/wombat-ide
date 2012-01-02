package wombat.scheme.library.base;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeNotImplementedError;
import wombat.scheme.values.*;

public class Eval {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		env.defineProcedure(new SchemeProcedure("eval") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("scheme-report-version") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("null-environment") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("environment") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("interaction-environment") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeNotImplementedError(this);
			}
		});
	}
}
