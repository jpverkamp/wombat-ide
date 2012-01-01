package wombat.scheme.continuations;

import wombat.scheme.values.*;

public abstract class Continuation extends SchemeProcedure {
	private static final long serialVersionUID = 3616375635854863679L;

	Continuation K;
	
	public Continuation(Continuation k) {
		K = k;
	}
}
