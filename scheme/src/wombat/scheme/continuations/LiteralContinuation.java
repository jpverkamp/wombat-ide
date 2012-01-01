package wombat.scheme.continuations;

import wombat.scheme.values.SchemeObject;

public class LiteralContinuation extends Continuation {
	private static final long serialVersionUID = -3736352394713034742L;
	
	SchemeObject<?> LiteralValue;
	
	public LiteralContinuation(Continuation k, SchemeObject<?> v) {
		super(k);
		LiteralValue = v;
	}

	@Override
	public SchemeObject<?> apply(SchemeObject<?>... args) {
		return K.apply(LiteralValue);
	}
}
