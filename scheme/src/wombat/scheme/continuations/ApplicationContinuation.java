package wombat.scheme.continuations;

import wombat.scheme.*;
import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.values.*;

public class ApplicationContinuation extends Continuation {
	private static final long serialVersionUID = -6256478235527515399L;

	SchemeObject<?>[] Values;
	Environment Env;
	int Position;
	
	public ApplicationContinuation(Continuation k, SExpression[] args, Environment env) {
		super(k);
		Values = args;
		Env = env;
		Position = 0;
	}

	@Override
	public SchemeObject<?> apply(SchemeObject<?>... args) {
		// TODO: Sanity check on length?
		
		// Evaluate the rator, make sure that it's a procedure.
		if (Position == 0) {
			SchemeObject<?> rator = Evaluator.evaluate((SExpression) Values[0], Env);
			
			if (rator instanceof SchemeProcedure) {
				Values[0] = rator;
				Position += 1;
				return this;
			}
				
			throw new SchemeRuntimeError(Values[0], this, rator.display() + " is not a procedure");
		}
		
		// Evaluate the arguments, one at a time
		else if (Position < Values.length) {
			Values[Position] = Evaluator.evaluate((SExpression) Values[Position], Env);
			Position += 1;
		}
		
		// Otherwise, we have everything we need. Apply it.
		else {
			
		}
		
		// TODO Auto-generated method stub
		return null;
	}
}
