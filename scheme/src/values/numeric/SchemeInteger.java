package values.numeric;

import java.math.BigInteger;

public class SchemeInteger extends SchemeNumber<BigInteger> {
	private static final long serialVersionUID = 6215884184891117457L;

	public SchemeInteger(BigInteger value) {
		super(value);
	}
	
	public SchemeInteger(String value) {
		super(new BigInteger(value));
	}
}
