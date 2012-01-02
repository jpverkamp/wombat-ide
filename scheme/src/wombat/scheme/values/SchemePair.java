package wombat.scheme.values;

import java.util.*;

import wombat.scheme.errors.SchemeRuntimeError;

public class SchemePair extends SchemeObject<Pair> {
	private static final long serialVersionUID = -4995393512892288973L;

	/**
	 * Create a new pair from car and cdr.
	 * @param car The first item.
	 * @param cdr The rest of the items.
	 */
	public SchemePair(SchemeObject<?> car, SchemeObject<?> cdr) {
		super(new Pair(car, cdr));
	}
	
	/**
	 * Generate a list from a java array.
	 * @param ls The 'list' to make into a list.
	 */
	public static SchemeObject<?> fromList(SchemeObject<?>[] ls) {
		if (ls.length == 0)
			return SchemeEmptyList.singleton();
		
		SchemeObject<?> cdr = SchemeEmptyList.singleton();
		for (int i = ls.length - 1; i >= 1; i--)
			cdr = new SchemePair(ls[i], cdr);
		
		return new SchemePair(ls[0], cdr);
	}
	
	/**
	 * Generate a java array from a list.
	 * @return An equivalent array.
	 */
	public SchemeObject<?>[] toList() {
		List<SchemeObject<?>> ls = new ArrayList<SchemeObject<?>>();
		ls.add(Value.Car);
		
		SchemeObject<?> cdr = Value.Cdr;
		do {
			ls.add(((SchemePair) cdr).Value.Car);
			cdr = ((SchemePair) cdr).Value.Cdr;
		} while (cdr instanceof SchemePair);
		
		if (cdr instanceof SchemeEmptyList) {
			SchemeObject<?>[] result = new SchemeObject<?>[ls.size()];
			for (int i = 0; i < result.length; i++)
				result[i] = ls.get(i);
			return result;
		} else {
			throw new SchemeRuntimeError(this, "Is not a proper list");
		}
	}
	
	/**
	 * Accessor for the car.
	 * @return The car.
	 */
	public SchemeObject<?> getCar() {
		return Value.Car;
	}
	
	/**
	 * Accessor for the cdr.
	 * @return The cdr.
	 */
	public SchemeObject<?> getCdr() {
		return Value.Cdr;
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		StringBuilder sb = new StringBuilder();
		sb.append("(");
		sb.append(Value.Car.display());
		sb.append(" ");
		SchemeObject<?> cdr = Value.Cdr;
		
		// Cdr down the list.
		while (cdr instanceof SchemePair) {
			sb.append(((SchemePair) cdr).Value.Car.display());
			sb.append(" ");
			cdr = ((SchemePair) cdr).Value.Cdr;
		}
		
		// If there's an empty list, it's a proper list, remove the last extra space.
		if (cdr instanceof SchemeEmptyList) {
			sb.delete(sb.length() - 1, sb.length());
		}
		
		// Otherwise, it's improper. Add the dot and last item.
		else {
			sb.append(". ");
			sb.append(cdr.display());
		}
		
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		StringBuilder sb = new StringBuilder();
		sb.append("(");
		sb.append(Value.Car.write());
		sb.append(" ");
		SchemeObject<?> cdr = Value.Cdr;
		
		// Cdr down the list.
		while (cdr instanceof SchemePair) {
			sb.append(((SchemePair) cdr).Value.Car.write());
			sb.append(" ");
			cdr = ((SchemePair) cdr).Value.Cdr;
		}
		
		// If there's an empty list, it's a proper list, remove the last extra space.
		if (cdr instanceof SchemeEmptyList) {
			sb.delete(sb.length() - 1, sb.length());
		}
		
		// Otherwise, it's improper. Add the dot and last item.
		else {
			sb.append(". ");
			sb.append(cdr.write());
		}
		
		sb.append(")");
		return sb.toString();
	}
	
	/**
	 * Override because it doesn't use values.
	 */
	public String toString() {
		return display();
	}
}

/**
 * Internal pair.
 */
class Pair {
	SchemeObject<?> Car;
	SchemeObject<?> Cdr;
	
	public Pair(SchemeObject<?> car, SchemeObject<?> cdr) {
		Car = car;
		Cdr = cdr;
	}
	
	public boolean equals(Object that) {
		return (that instanceof Pair
				&& Car.equals(((Pair) that).Car)
				&& Cdr.equals(((Pair) that).Cdr));
	}
}
