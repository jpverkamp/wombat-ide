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
	public void addMethods(KawaWrap kawa) throws Throwable {
		kawa.eval("(define (iota n) (define (^ i) (if (= n i) '() (cons i (^ (add1 i))))) (^ 0))");
		
    	kawa.bind(new Procedure1("random") {
			@Override
			public Object apply1(Object max) throws Throwable {
				if (max instanceof IntNum) {
					IntNum i = (IntNum) max;
					if (i.isNegative())
						throw new Exception("random expected a positive integer, got " + KawaWrap.formatObject(max));
						
					
					if (i.inIntRange())
						return new IntNum(random.nextInt(((IntNum) max).ival));
					else
					{
						BigInteger bi = i.asBigInteger();
						BigInteger r;
						
						do {
						    r = new BigInteger(bi.bitLength(), random);
						} while (r.compareTo(bi) >= 0);
						
						return IntNum.asIntNumOrNull(r);
						
					}
						
				} else
					throw new Exception("random expected an integer, got " + KawaWrap.formatObject(max));
			}
        });
	}
}
