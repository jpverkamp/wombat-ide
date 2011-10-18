package globals;

import java.math.BigInteger;
import java.util.Random;

import util.KawaWrap;

import gnu.mapping.Procedure1;
import gnu.math.IntNum;

public class WRandom extends Globals {
	Random random = new Random();
	
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 */
	@Override
	public void addMethods(final KawaWrap kawa) throws Throwable {
		kawa.eval("(define (iota n) (define (^ i) (if (= n i) '() (cons i (^ (add1 i))))) (^ 0))");
		
    	kawa.bind(new Procedure1("random") {
			@Override
			public Object apply1(Object max) throws Throwable {
				long maxv = 0;
				if (max instanceof Integer) maxv = new Long("" + (Integer) max);
				else if (max instanceof Long) maxv = (Long) max;
				else if (max instanceof IntNum) {
					if (((IntNum) max).inLongRange())
						maxv = ((IntNum) max).longValue();
					else {
						IntNum i = (IntNum) max;
						if (i.isNegative())
							throw new IllegalArgumentException("random expected a positive integer, got " + KawaWrap.formatObject(max));
						
						BigInteger bi = i.asBigInteger();
						BigInteger r;
						
						do {
						    r = new BigInteger(bi.bitLength(), random);
						} while (r.compareTo(bi) >= 0);
						
						return IntNum.asIntNumOrNull(r);
					}
				} else {
					throw new IllegalArgumentException("random expected an integer, got " + KawaWrap.formatObject(max));
				}
				
				return (Math.abs(random.nextLong()) % maxv); 
			}
        });
	}
}
