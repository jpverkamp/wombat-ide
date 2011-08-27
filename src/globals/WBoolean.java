package globals;

import util.KawaWrap;

public class WBoolean extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(KawaWrap kawa) throws Throwable {
    	kawa.eval("(define true #t)");
    	kawa.eval("(define false #f)");
	}
}
