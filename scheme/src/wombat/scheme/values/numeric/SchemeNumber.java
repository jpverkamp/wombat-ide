package wombat.scheme.values.numeric;

import java.math.BigDecimal;
import java.math.BigInteger;


import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.SchemeObject;

/**
 * Represents the numeric tower.
 * 
 * @param <T> A type of Java number.
 */
public abstract class SchemeNumber<T extends Number> extends SchemeObject<T> { 
	private static final long serialVersionUID = 7750105092519285421L;
	
	@SuppressWarnings({ "rawtypes" })
	static final Class[] tower = new Class[] {
		SchemeInteger.class,
		SchemeRational.class,
		SchemeReal.class,
		SchemeComplex.class,
	};

	/**
	 * Create a new number.
	 * @param value The Java value to store.
	 */
	public SchemeNumber(T value) {
		super(value);
	}
	
	/**
	 * Add another number to this.
	 */
	public SchemeNumber<?> add(SchemeNumber<?> that) {
		Class<? extends SchemeNumber<?>> to = matchTypes(this, that);
		return this.convert(to)._add(that.convert(to)).simplify();
	}
	abstract SchemeNumber<?> _add(SchemeNumber<?> that);
	
	/**
	 * Match the types of two Scheme numbers.
	 * @param a The first number.
	 * @param b The second number.
	 * @return The result.
	 */
	@SuppressWarnings("unchecked")
	public static Class<? extends SchemeNumber<?>> matchTypes(SchemeNumber<?> a, SchemeNumber<?> b) {
		if (a.getClass().equals(b.getClass()))
			return (Class<? extends SchemeNumber<?>>) a.getClass();
		
		for (int i = 0; i < tower.length; i++)
			for (int j = i + 1; j < tower.length; j++)
				if (a.getClass().equals(tower[i]) && b.getClass().equals(tower[j]))
					return tower[j];
		
		
		for (int i = 0; i < tower.length; i++)
			for (int j = i + 1; j < tower.length; j++)
				if (b.getClass().equals(tower[i]) && a.getClass().equals(tower[j]))
					return tower[j];
		
		// this shouldn't happen
		return null;
	}
	
	/**
	 * Convert to a different type of Scheme number.
	 * Throws a SchemeRuntimeException if it's not possible.
	 * 
	 * @param to The type to convert to.
	 * @return The new Scheme number.
	 */
	public SchemeNumber<?> convert(Class<? extends SchemeNumber<?>> to) {
		if (this.getClass().equals(to))
			return this;
		
		if (this instanceof SchemeInteger) {
			if (to == SchemeRational.class)
				return new SchemeRational((BigInteger) Value, BigInteger.ONE);
			else if (to == SchemeReal.class)
				return new SchemeReal(Value.toString());
			else if (to == SchemeComplex.class)
				return new SchemeComplex(Value.toString());
		} else if (this instanceof SchemeRational) {
			if (to == SchemeReal.class)
				return new SchemeReal(
						new BigDecimal(((SchemeRational) this).Value.Numerator.toString()).divide(
								new BigDecimal(((SchemeRational) this).Value.Denomiator.toString())));
			else if (to == SchemeComplex.class)
				return new SchemeComplex((BigDecimal) this.convert(SchemeReal.class).Value, BigDecimal.ZERO);
		} else if (this instanceof SchemeReal) {
			if (to == SchemeComplex.class)
				return new SchemeComplex((BigDecimal) this.Value, BigDecimal.ZERO);
		}
			
		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("Unable to convert ");
		stringBuilder.append(this.getClass().getName());
		stringBuilder.append(" to ");
		stringBuilder.append(to.toString());
		
		throw new SchemeRuntimeError(this, (stringBuilder.toString()).replace("Scheme", ""));
	}
	
	/**
	 * Simplify a number (if possible)
	 * @return The number unchanged or a simpler version.
	 */
	public SchemeNumber<?> simplify() {
		if (this instanceof SchemeRational && ((SchemeRational) this).Value.Denomiator.equals(BigInteger.ONE))
			return new SchemeInteger(((SchemeRational) this).Value.Numerator);
		else if (this instanceof SchemeComplex && ((SchemeComplex) this).Value.Imagionary.equals(BigDecimal.ZERO))
			return new SchemeReal(((SchemeComplex) this).Value.Real);
		else
			return this;
	}
}
