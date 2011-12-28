package values.numeric;

import java.math.BigInteger;

public class SchemeRational extends SchemeNumber<BigRational> {
	private static final long serialVersionUID = -4295784075137351641L;

	public SchemeRational(BigRational value) {
		super(value);
	}
	
	public SchemeRational(BigInteger numer, BigInteger denom) {
		super(new BigRational(numer, denom));
	}
	
	public SchemeRational(String value) {
		super(null);
		
		if (value.contains("/")) {
			String[] parts = value.split("/");
			Value = new BigRational(new BigInteger(parts[0]), new BigInteger(parts[1]));
		} else {
			Value = new BigRational(new BigInteger(value), BigInteger.ONE);
		}
	}
}

class BigRational extends Number {
	private static final long serialVersionUID = -6253808909145586931L;
	
	BigInteger Numerator;
	BigInteger Denomiator;
	
	public BigRational(BigInteger numer, BigInteger denom) {
		Numerator = numer;
		Denomiator = denom;
	}

	public double doubleValue() {
		return Numerator.doubleValue() / Denomiator.doubleValue();
	}

	
	public float floatValue() {
		return Numerator.floatValue() / Denomiator.floatValue();
	}

	
	public int intValue() {
		return Numerator.intValue() / Denomiator.intValue();
	}

	public long longValue() {
		return Numerator.longValue() / Denomiator.longValue();
	}
}