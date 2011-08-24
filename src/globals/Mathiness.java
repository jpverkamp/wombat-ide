package globals;

import kawa.standard.Scheme;

public class Mathiness extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(Scheme kawa) throws Throwable {
    	kawa.eval("(define (sub1 n) (- n 1))");
    	kawa.eval("(define (add1 n) (+ n 1))");
	}
}
