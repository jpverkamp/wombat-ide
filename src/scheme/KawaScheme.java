package scheme;

import util.OutputIntercept;

public class KawaScheme extends Scheme implements Runnable {
	/**
	 * Create a new scheme.
	 */
	public KawaScheme() {
		Thread t = new Thread(this);
        t.setDaemon(true);
        t.start();
	}

	public void run() {
        // Connect to Kawa (side note: awesome)
		kawa.standard.Scheme kawa = new kawa.standard.Scheme();

		// Process commands.
		while (true) {
            try {
            	while (!commands.isEmpty()) {
                    Object result = kawa.eval(commands.poll());

                    if (result.toString().length() == 0)
                    	responses.add(null);
                    
                    responses.add(result.toString());
                }
            	
            	if (OutputIntercept.hasContent())
            		responses.add(OutputIntercept.getContent());

                Thread.sleep(50);
            }

            // Report problems with evaluation.
            catch (Throwable ex) {
                responses.add(ex.getMessage());
            }
        }
	}

}
