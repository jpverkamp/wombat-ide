package globals;

import util.KawaWrap;

/**
 * Represent global functions or other constants to add to a Kawa instance.
 */
public abstract class Globals {
	/**
	 * Add any methods this class wants to define.
	 * @param kawa The scheme to add the methods to.
	 */
	public abstract void addMethods(KawaWrap kawa) throws Throwable;
}
