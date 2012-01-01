package wombat.scheme.values.numeric;

import java.math.BigInteger;

public class SchemeInteger extends SchemeNumber<BigInteger> {
	private static final long serialVersionUID = 6215884184891117457L;

	public SchemeInteger(int value) {
		super(new BigInteger("" + value));
	}
	
	public SchemeInteger(long value) {
		super(new BigInteger("" + value));
	}
	
	public SchemeInteger(BigInteger value) {
		super(value);
	}
	
	public SchemeInteger(String value) {
		super(new BigInteger(value));
	}
	
	public SchemeNumber<?> _add(SchemeNumber<?> that) {
		return new SchemeInteger(Value.add(((SchemeInteger) that).Value));
	}
}
