package globals;

import util.KawaWrap;

public class WDefine extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 */
	@Override
	public void addMethods(KawaWrap kawa) throws Throwable {
    	kawa.eval("(set! $define$ define)");
    	kawa.eval(
			"(define-syntax define" + 
			"  (syntax-rules (lambda)" + 
			"    [(define name" + 
			"       (lambda (args ...)" + 
			"         bodies ...))" + 
			"     ($define$ (name args ...)" + 
			"       bodies ...)]" + 
			"    [(define name" + 
			"       (lambda (args ... . dotted)" + 
			"         bodies ...))" + 
			"     ($define$ (name args ... . dotted)" + 
			"       bodies ...)]" + 
			"    [(define name" + 
			"       (lambda arg" + 
			"         bodies ...))" + 
			"     ($define$ (name . arg)" + 
			"       bodies ...)]" + 
			"    [(define stuff ...)" + 
			"     ($define$ stuff ...)]))");
	}
}
