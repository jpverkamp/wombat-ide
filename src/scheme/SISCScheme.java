package scheme;

import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;

import java.io.IOException;

/**
 * Use SISC for Scheme.
 */
public class SISCScheme extends Scheme implements Runnable {
    Interpreter interpreter;

    /**
     * Create a new scheme.
     */
    public SISCScheme() {
        new Thread(this).start();
    }

    /**
     * Run the thread.
     */
    public void run() {
        // Create the interpreter.
        try {
            AppContext ctx = new AppContext();
            ctx.addDefaultHeap();
            interpreter = Context.enter(ctx);
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Deal with input.
        while (true) {
            try {
                while (!commands.isEmpty()) {
                    Value v = interpreter.eval(commands.remove());
                    responses.add(v.toString());
                }

                Thread.sleep(50);
            }

            // Report problems with evaluation.
            catch (IOException ioe) {
                responses.add(ioe.getMessage());
                System.err.println(ioe.getMessage());
            } catch (SchemeException se) {
                responses.add(se.getMessageText());
                System.err.println(se.getMessageText());
            }

            // We really don't care if this happens. Just keep going.
            // Actually, does this ever happen? Really?
            catch (InterruptedException ie) {

            }
        }
    }
}