package wombat.scheme.values;

import java.math.BigInteger;
import java.util.Arrays;

import wombat.scheme.values.numeric.SchemeInteger;

public class SchemeVector extends SchemeObject<SchemeObject<?>[]> {
	private static final long serialVersionUID = -6657528989854568132L;

	/**
	 * Create an empty vector (defaults to storing zero in each cell).
	 * @param size The size of the empty vector to create.
	 */
	public SchemeVector(int size) {
		super(new SchemeObject<?>[size]);
		
		for (int i = 0; i < size; i++)
			Value[i] = new SchemeInteger(BigInteger.ZERO);
	}
	
	/**
	 * Create a new vector containing the specified values.
	 * @param values The array of values to store.
	 */
	public SchemeVector(SchemeObject<?>[] values) {
		super(values);
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		StringBuilder sb = new StringBuilder();
		sb.append("#(");
		for (SchemeObject<?> each : Value) {
			sb.append(each.display());
			sb.append(" ");
		}
		sb.delete(sb.length() - 1, sb.length());
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		StringBuilder sb = new StringBuilder();
		sb.append("#(");
		for (SchemeObject<?> each : Value) {
			sb.append(each.write());
			sb.append(" ");
		}
		sb.delete(sb.length() - 1, sb.length());
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Override to debug-print vectors.
	 */
	public String toString() {
		return "Vector" + Arrays.toString(Value);
	}
}
