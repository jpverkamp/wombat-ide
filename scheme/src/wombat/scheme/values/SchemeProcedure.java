package wombat.scheme.values;

import java.util.Arrays;

import wombat.scheme.errors.*;

/**
 * Creates a procedure.
 */
public abstract class SchemeProcedure extends SchemeObject<Object> {
	private static final long serialVersionUID = 6809643481710108526L;
	
	String Name;
	
	/**
	 * Create a new anonymous procedure.
	 */
	public SchemeProcedure() {
		super(null);
	}
	
	/**
	 * Create a new named procedure.
	 * @param name
	 */
	public SchemeProcedure(String name) {
		super(null);
		Name = name;
	}
	
	
	/**
	 * Verify an exact argument count.
	 * @param argCount The amount we got. 
	 * @param expectedArgCount The amount we expected.
	 */
	public void verifyExactArity(int argCount, int expectedArgCount) {
		if (argCount != expectedArgCount)
			throw new SchemeRuntimeError(this, "Incorrent number of arguments to " + write() + ", expected " + expectedArgCount + ", got " + argCount);
	}

	/**
	 * Verify a variable argument count.
	 * @param argCount THe amount we got.
	 * @param expectedMinArgCount The amount we expected (at minimum)
	 */
	public void verifyMinimumArity(int argCount, int expectedMinArgCount) {
		if (argCount < expectedMinArgCount)
			throw new SchemeRuntimeError(this, "Incorrent number of arguments to " + write() + ", expected at least " + expectedMinArgCount + ", got " + argCount);
	}
	
	/**
	 * Verify a variable arg count (from a case-lambda for example)
	 * @param argCount The number of arguments.
	 * @param possibleArgCounts The possible argument counts.
	 */
	public void verifyListArity(int argCount, int... possibleArgCounts) {
		for (int possibleArgCount : possibleArgCounts)
			if (argCount == possibleArgCount)
				return;
		
		throw new SchemeRuntimeError(this, "Incorrent number of arguments to " + write() + ", expected " + Arrays.toString(possibleArgCounts) + ", got " + argCount);
	}
	
	/**
	 * Verify the type of an argument.
	 * @param n
	 * @param arg
	 * @param expected
	 */
	public void verifyTypeOf(int n, SchemeObject<?> arg, Class<? extends SchemeObject<?>> expected) {
		if (!expected.isInstance(arg))
			throw new SchemeRuntimeError(this, "Argument " + n + " expected " + expected.getName().replace(".numeric", "").replace("wombat.scheme.values.Scheme", "") + ", got " + arg.getSchemeType() + " '" + arg.display() + "'");
	}
	
	/**
	 * Apply the procedure.
	 * 
	 * @param k What to do after applying the function.
	 * @param args Arguments to the function.
	 */
	public abstract SchemeObject<?> apply(SchemeObject<?>... args);
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		return write();
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		if (Name == null)
			return "#<procedure>";
		else
			return "#<procedure " + Name + ">";
	}
	
	/**
	 * Better debugging because there are a lot of these.
	 */
	public String toString() {
		return write();
	}

	/**
	 * Get this procedure's name.
	 * @return The name.
	 */
	public String getName() {
		return Name;
	}
}
