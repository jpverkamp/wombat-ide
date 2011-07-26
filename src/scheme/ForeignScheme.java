package scheme;

import java.io.*;

/**
 * Use a foreign process for Scheme.
 */
public class ForeignScheme extends Scheme implements Runnable {
    String ProcessPath;
    Process CurrentProcess;

    /**
     * Create a new scheme.
     *
     * @param scheme The name of the scheme defined in schemes.cfg.
     */
    public ForeignScheme(String scheme) throws FileNotFoundException {

        System.out.println("ForeignScheme loading."); // TODO: Debug

        // Load the configuration file that defines Scheme implementations.
        Scheme s = new SISCScheme();
        s.doStringAndWait("(define schemes '())");
        s.doStringAndWait("(define (scheme name . paths) (set! schemes (cons (list name paths) schemes)))");
        s.doFileAndWait("schemes.cfg");

        // See if we can find it.
        String paths = s.doStringAndWait("(assoc '" + scheme + " schemes)");
        if ("#f".equals(paths))
            throw new FileNotFoundException("Cannot connect to " + scheme + ", not defined.");
        else {
            paths = s.doStringAndWait("(cdr '" + paths + ")");
            paths = paths.substring(3, paths.length() - 3);

            for (String path : paths.split("\" \"")) {
                File f = new File(path);
                if (f.exists()) {
                    ProcessPath = path;
                    Thread t = new Thread(this);
                    t.setDaemon(true);
                    t.start();

                    System.out.println("ForeignScheme loaded."); // TODO: Debug
                    return;
                }
            }

            throw new FileNotFoundException("Cannot connect to " + scheme + ", no valid paths found.");
        }
    }

    /**
     * Run the thread.
     */
    public void run() {
        // Streams connected to the process.
        final BufferedReader output;
        final BufferedWriter input;

        // TODO: Connect to the interpreter.
        try {
            ProcessBuilder pb = new ProcessBuilder(ProcessPath);
            pb.redirectErrorStream(true);
            CurrentProcess = pb.start();
            output = new BufferedReader(new InputStreamReader(CurrentProcess.getInputStream()));
            input = new BufferedWriter(new OutputStreamWriter(CurrentProcess.getOutputStream()));

            // Clear the initial version information and what not.
            Thread t = new Thread(new Runnable() {
                public void run() {
                    String line;
                    while (true) {
                        try {
                            Thread.sleep(50);
                            while ((line = output.readLine()) != null)
                                responses.offer(line);
                        } catch (IOException ioe) {
                            ioe.printStackTrace();
                        } catch (InterruptedException ie) {
                            // Don't care.
                        }
                    }
                }
            });
            t.setDaemon(true);
            t.start();

            // Deal with input.
            while (true) {
                while (!commands.isEmpty()) {
                    input.write(commands.poll() + "\n");
                    input.flush();
                }
                Thread.sleep(50);
            }
        } catch (IOException ioe) {
            // TODO: Deal with this.
        }
        // We really don't care if this happens. Just keep going.
        // Actually, does this ever happen? Really?
        catch (InterruptedException ie) {

        }
    }
}