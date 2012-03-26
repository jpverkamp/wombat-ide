/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.util;

import java.util.*;

/**
 * A length where new items get rid of the oldest.
 * 
 * NOTE: Only implemented as much as I needed. It's likely badly broken. :)
 * 
 * @param <E> The type of things in the list.
 */
public class FixedLengthList<E> extends ArrayList<E> {
	private static final long serialVersionUID = 2424060477353268513L;

	int MaxSize;
	
	public FixedLengthList(int maxSize) {
		MaxSize = maxSize;
	}
	
	@Override public boolean add(E o) {
		boolean result = super.add(o);
		while (size() > MaxSize) remove(0);
		return result;
	}
}
	
	