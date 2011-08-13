package scheme;

import gui.ErrorFrame;
import util.OutputIntercept;
import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeException;

import java.io.IOException;

import sisc.data.SchemeVoid;
import sisc.ser.MemoryRandomAccessInputStream;

/**
 * Use SISC for Scheme.
 */
public class SISCScheme extends Scheme implements Runnable {
    Interpreter interpreter;

    /**
     * Create a new scheme.
     */
    public SISCScheme() {
    	Thread t = new Thread(this);
        t.setDaemon(true);
        t.start();
    }

    /**
     * Run the thread.
     */
    @Override
    public void run() {
        // Create the interpreter.
        try {
            AppContext ctx = new AppContext();
            ctx.addHeap(new MemoryRandomAccessInputStream(getClass().getResourceAsStream("/sisc.shp")));
            interpreter = Context.enter(ctx);
            
            OutputIntercept.enable();
        } catch (ClassNotFoundException ex) {
            ErrorFrame.log("Unable to initialize SISC: Cannot find heap.");
        } catch (IOException ex) {
            ErrorFrame.log("Unable to initialize SISC: Cannot load heap.");
        }
        
        // Start a thread to read anything that gets written to standard out or error.
        Thread t = new Thread(new Runnable() {
            public void run() {
                while (true) {
                	if (OutputIntercept.hasContent())
                		responses.add(OutputIntercept.getContent());
                	
                    try { Thread.sleep(50); } catch (InterruptedException ie) {}
                }
            }
        });
        t.setDaemon(true);
        t.start();

        // Deal with input.
        while (true) {
            try {
                while (!commands.isEmpty()) {
                    Value v = interpreter.eval(commands.remove());
                    if ((v instanceof SchemeVoid))
                        responses.add(null);
                    else
                        responses.add(v.toString());
                }

                Thread.sleep(50);
            }

            // Report problems with evaluation.
            catch (IOException ioe) {
                responses.add(ioe.getMessage());
            } catch (SchemeException se) {
                responses.add(se.getMessageText());
            }

            // We really don't care if this happens. Just keep going.
            // Actually, does this ever happen? Really?
            catch (InterruptedException ie) {

            }
        }
    }
}