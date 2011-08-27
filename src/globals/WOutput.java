package globals;

import util.KawaWrap;

public class WOutput extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(KawaWrap kawa) throws Throwable {
    	kawa.eval("(current-output-port (util.OutputIntercept:me))");
    	kawa.eval("(current-error-port (util.OutputIntercept:me))");
	}
}
