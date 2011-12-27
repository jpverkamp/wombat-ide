package wombat.scheme.values.numeric;

import java.math.BigDecimal;

public class SchemeReal extends SchemeNumber<BigDecimal> {
	private static final long serialVersionUID = 4021708626582263207L;

	public SchemeReal(BigDecimal value) {
		super(value);
	}
	
	public SchemeReal(String value) {
		super(new BigDecimal(value));
	}
	
	public SchemeNumber<?> _add(SchemeNumber<?> that) {
		return new SchemeReal(Value.add(((SchemeReal) that).Value));
	}
}
