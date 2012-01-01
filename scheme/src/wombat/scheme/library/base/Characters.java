package wombat.scheme.library.base;

import java.math.BigInteger;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.*;
import wombat.scheme.values.numeric.*;

public class Characters {
	@SuppressWarnings("serial")
	public static void load(Environment env) {
		// (char? x) returns #t if x is a character, #f otherwise
		env.defineProcedure(new SchemeProcedure("char?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				return new SchemeBoolean(args[0] instanceof SchemeCharacter);
			}
		});
		
		// (char* x y z ...) return #t if x y z ... are ordered by the given predicate
		// use unicode ordering
		env.defineProcedure(new SchemeProcedure("char=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(((SchemeCharacter) args[i]).getValue()) != 
							Character.getNumericValue(((SchemeCharacter) args[i + 1]).getValue()))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char<?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(((SchemeCharacter) args[i]).getValue()) >= 
							Character.getNumericValue(((SchemeCharacter) args[i + 1]).getValue()))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char>?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(((SchemeCharacter) args[i]).getValue()) <= 
							Character.getNumericValue(((SchemeCharacter) args[i + 1]).getValue()))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char<=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(((SchemeCharacter) args[i]).getValue()) > 
							Character.getNumericValue(((SchemeCharacter) args[i + 1]).getValue()))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char>=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(((SchemeCharacter) args[i]).getValue()) < 
							Character.getNumericValue(((SchemeCharacter) args[i + 1]).getValue()))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		// (char-ci* x y z ...) return #t if x y z ... are ordered by the given predicate (case-insensitive)
		// use unicode ordering
		env.defineProcedure(new SchemeProcedure("char-ci=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(Character.toLowerCase(Character.toLowerCase(((SchemeCharacter) args[i]).getValue()))) != 
							Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i + 1]).getValue())))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char-ci<?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i]).getValue())) >= 
							Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i + 1]).getValue())))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char-ci>?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i]).getValue())) <= 
							Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i + 1]).getValue())))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char-ci<=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i]).getValue())) > 
							Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i + 1]).getValue())))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		env.defineProcedure(new SchemeProcedure("char-ci>=?") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyMinimumArity(args.length, 2);
				
				for (int i = 0; i < args.length; i++)
					verifyTypeOf(i + 1, args[i], SchemeCharacter.class);
				
				for (int i = 0; i < args.length - 1; i++)
					if (Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i]).getValue())) < 
							Character.getNumericValue(Character.toLowerCase(((SchemeCharacter) args[i + 1]).getValue())))
						return new SchemeBoolean(false);
				
				return new SchemeBoolean(true);
			}
		});
		
		// (digit-value x) returns the numeric value for 0-9, #f otherwise
		env.defineProcedure(new SchemeProcedure("digit-value") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeCharacter.class);
				
				if (Character.isDigit(((SchemeCharacter) args[0]).getValue()))
					return new SchemeInteger(args[0].display());
				else
					return new SchemeBoolean(false);
			}
		});
		
		// (char->integer x) gets the unicode codepoint for a character
		env.defineProcedure(new SchemeProcedure("char->integer") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeCharacter.class);
				return new SchemeInteger(Character.getNumericValue(((SchemeCharacter) args[0]).getValue()));
			}
		});
		
		// (integer->char x) turns a unicode codepoint into a character
		env.defineProcedure(new SchemeProcedure("integer->char") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeInteger.class);
				
				if (((SchemeInteger) args[0]).getValue().compareTo(new BigInteger("ffff", 16)) < 0)
					return new SchemeCharacter((char) ((SchemeInteger) args[0]).getValue().intValue());
				else
					throw new SchemeRuntimeError(this, args[0].display() + " is out of range");
			}
		});
		
		// (char-upcase c) converts a character into its uppercase equivalent
		env.defineProcedure(new SchemeProcedure("char-upcase") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeCharacter.class);
				return new SchemeCharacter(Character.toUpperCase((((SchemeCharacter) args[0]).getValue())));
			}
		});
		
		// (char-downcase c) converts a character into its lowercase equivalent
		env.defineProcedure(new SchemeProcedure("char-downcase") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeCharacter.class);
				return new SchemeCharacter(Character.toLowerCase((((SchemeCharacter) args[0]).getValue())));
			}
		});
		
		// (char-foldcase c) converts a character into its foldcase equivalent
		env.defineProcedure(new SchemeProcedure("char-foldcase") {
			public SchemeObject<?> apply(SchemeObject<?>... args) {
				verifyExactArity(args.length, 1);
				verifyTypeOf(1, args[0], SchemeCharacter.class);
				return new SchemeCharacter(Character.toLowerCase((((SchemeCharacter) args[0]).getValue())));
			}
		});
	}
}
