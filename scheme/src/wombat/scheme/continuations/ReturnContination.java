package wombat.scheme.continuations;

import wombat.scheme.values.SchemeObject;

public class ReturnContination extends Continuation {
	private static final long serialVersionUID = -6543090785455600308L;

	public ReturnContination() {
		super(null);
	}

	@Override
	public SchemeObject<?> apply(SchemeObject<?>... args) {
		// TODO: Sanity checks
		return args[0];
	}
}
