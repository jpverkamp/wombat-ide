package wombat.scheme.library.base;

import wombat.scheme.Environment;
import wombat.scheme.values.SchemeObject;
import wombat.scheme.values.SchemeProcedure;
import wombat.scheme.values.SchemeString;

public class InputAndOutput {
	private InputAndOutput() {}
	
	public static void load(Environment env) {
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
