package globals;

import java.util.Random;

import gnu.mapping.Procedure1;
import gnu.math.IntNum;
import kawa.standard.Scheme;

public class Randomness extends Globals {
	Random random = new Random();
	
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 */
	@Override
	public void addMethods(Scheme kawa) throws Throwable {
    	kawa.defineFunction(new Procedure1("random") {
			@Override
			public Object apply1(Object max) throws Throwable {
				if (max instanceof IntNum)
					return new IntNum(random.nextInt(((IntNum) max).ival));
				else
					throw new Exception("random expected an integer, got " + max);
			}
        });
	}
}
