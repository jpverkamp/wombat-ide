package wombat.scheme.values.numeric;

import java.math.BigDecimal;

public class SchemeComplex extends SchemeNumber<BigComplex> {
	private static final long serialVersionUID = -8994511477699850178L;

	public SchemeComplex(BigComplex value) {
		super(value);
	}
	
	public SchemeComplex(BigDecimal real, BigDecimal imag) {
		super(new BigComplex(real, imag));
	}
	
	public SchemeComplex(String value) {
		super(null);
		
		if (value.contains("+")) {
			String[] parts = value.replace("i", "").split("+");
			Value = new BigComplex(new BigDecimal(parts[0]), new BigDecimal(parts[1]));
		} else if (value.contains("-")) {
				String[] parts = value.replace("i", "").split("-");
				Value = new BigComplex(new BigDecimal(parts[0]), new BigDecimal(parts[1]).negate());
		} else {
			Value = new BigComplex(new BigDecimal(value), BigDecimal.ZERO);
		} 
	}
}

class BigComplex extends Number {
	private static final long serialVersionUID = -6253808909145586931L;
	
	BigDecimal Real;
	BigDecimal Imagionary;
	
	public BigComplex(BigDecimal real, BigDecimal imag) {
		Real = real;
		Imagionary = imag;
	}

	public double doubleValue() {
		return Real.doubleValue();
	}

	
	public float floatValue() {
		return Real.floatValue();
	}

	
	public int intValue() {
		return Real.intValue();
	}

	public long longValue() {
		return Real.longValue();
	}
}