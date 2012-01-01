package wombat.scheme.library.base;

import java.util.*;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.*;

public class ControlFeatures {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		// (procedure? x) return #t for procedures, #f otherwise
		env.defineProcedure(new SchemeProcedure("procedure?") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				return new SchemeBoolean(args[0] instanceof SchemeProcedure);
			}
		});
		
		// (apply x ls) applies a function f to a list as its arguments
		env.defineProcedure(new SchemeProcedure("apply") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 2);
				verifyTypeOf(1, args[0], SchemeProcedure.class);
				verifyTypeOf(2, args[1], SchemePair.class);

				return ((SchemeProcedure) args[0]).apply(((SchemePair) args[1]).toList());
			}
		});
		
		// (*map f ls1 ...) applies f to each item in the given lists
		// * could be nothing for lists, string, or vector
		final SchemeProcedure map = new SchemeProcedure("map") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				verifyTypeOf(1, args[0], SchemeProcedure.class);
				for (int i = 1; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemePair.class);
				
				SchemeObject<?>[] lss = new SchemeObject<?>[args.length - 1];
				for (int i = 0; i < lss.length; i++)
					lss[i] = (SchemeObject<?>) args[i + 1];
				
				List<SchemeObject<?>> results = new ArrayList<SchemeObject<?>>();
				SchemeObject<?>[] current = new SchemeObject<?>[lss.length];
				while (true) {
					// Copy parameters over, stop if any of the lists are empty.
					for (int i = 0; i < lss.length; i++) {
						// End of lists, return
						if (lss[i] instanceof SchemeEmptyList) {
							SchemeObject<?>[] ls = new SchemeObject<?>[results.size()];
							for (int j = 0; j < ls.length; j++)
								ls[j] = results.get(j);
							return SchemePair.fromList(ls);
						}
						
						// Valid item, add it
						else if (lss[i] instanceof SchemePair) {
							current[i] = ((SchemePair) lss[i]).getCar();
							lss[i] = ((SchemePair) lss[i]).getCdr();
						}
						
						// Improper list, error out
						else 
							throw new SchemeRuntimeError(this, "Argument " + (i + 1) + " is not a proper list.");
					}
					
					// Apply it and add to the result
					results.add(((SchemeProcedure) args[0]).apply(current));
				}
			}
		};
		env.defineProcedure(map);
		
		final SchemeProcedure stringMap = new SchemeProcedure("string-map") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				verifyTypeOf(1, args[0], SchemeProcedure.class);
				for (int i = 1; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeString.class);
				
				SchemeString[] lss = new SchemeString[args.length - 1];
				for (int i = 0; i < lss.length; i++)
					lss[i] = (SchemeString) args[i + 1];
				
				StringBuilder result = new StringBuilder();
				
				// Loop across the strings in sync
				SchemeCharacter[] current = new SchemeCharacter[lss.length];
				for (int i = 0; ; i++) {
					// If we're still in each string, grab the next character
					for (int j = 0; j < lss.length; j++) {
						if (i < lss[j].getValue().length())
							current[j] = new SchemeCharacter(lss[j].getValue().charAt(i));
					
						// As soon as one ends, stop.
						else
							return new SchemeString(result.toString());
					}
					
					// Add the result of applying the function to the buffer
					SchemeObject<?> newchar = ((SchemeProcedure) args[0]).apply(current);
					if (newchar instanceof SchemeCharacter)
						result.append(((SchemeCharacter) newchar).getValue());
					else
						throw new SchemeRuntimeError(this, args[0].display() + " did not return a Character");
				}
			}
		};
		env.defineProcedure(stringMap);
		
		final SchemeProcedure vectorMap = new SchemeProcedure("vector-map") {
			@Override
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				verifyTypeOf(1, args[0], SchemeProcedure.class);
				for (int i = 1; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeVector.class);
				
				SchemeVector[] lss = new SchemeVector[args.length - 1];
				for (int i = 0; i < lss.length; i++)
					lss[i] = (SchemeVector) args[i + 1];
				
				List<SchemeObject<?>> result = new ArrayList<SchemeObject<?>>();

				// Loop across the vectors in sync
				SchemeObject<?>[] current = new SchemeObject<?>[lss.length];
				for (int i = 0; ; i++) {
					// If we're still in each string, grab the next character
					for (int j = 0; j < lss.length; j++) {
						if (i < lss[j].getValue().length)
							current[j] = lss[j].getValue()[i];
					
						// As soon as one ends, stop.
						else
							return new SchemeString(result.toString());
					}

					// Add the result of applying the function to the buffer
					result.add(((SchemeProcedure) args[0]).apply(current));
				}
			}
		};
		env.defineProcedure(vectorMap);
		
		// (*for-each f ls1 ...) applies f to each item in the given lists but doesn't return
		// * could be nothing for lists, string, or vector
		env.defineProcedure(new SchemeProcedure("for-each") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				map.apply(args);
				return SchemeVoid.singleton();
			}
		});
		
		env.defineProcedure(new SchemeProcedure("string-for-each") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				stringMap.apply(args);
				return SchemeVoid.singleton();
			}
		});
		
		env.defineProcedure(new SchemeProcedure("vector-for-each") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				vectorMap.apply(args);
				return SchemeVoid.singleton();
			}
		});
		
		// TODO: (call-with-current-continuation f)
		// alias: call/cc
		env.defineProcedure(new SchemeProcedure("call-with-current-continuation") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				throw new SchemeRuntimeError(this, "Sorry, call/cc is not possible in this system");
			}
		});
		env.define(new SchemeSymbol("call/cc"), env.get(new SchemeSymbol("call-with-current-continuation")));
	}
}
