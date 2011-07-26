package scheme;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Scanner;

/**
 * Wrap all of the details of a Scheme system.
 */
public abstract class Scheme {
    // Maintain these in subclasses.
    final Queue<String> commands = new LinkedList<String>();
    final Queue<String> responses = new LinkedList<String>();

    /**
     * Queue a string to run on the interpreter.
     *
     * @param input A string with Schemey goodness.
     */
    public void doString(String input) {
        commands.add(input);
    }


    /**
     * Do a command and return the wait for the next output to return.
     *
     * @param input The command to run.
     * @return The first line returned (NOTE: always a single line!)
     */
    public String doStringAndWait(String input) {
        doString(input);
        while (!hasResponse())
            try {
                Thread.sleep(50);
            } catch (InterruptedException ie) {
            }
        return nextResponse();
    }

    /**
     * Load a file containing Scheme code (bypasses "(load ...)")
     *
     * @param filename The filename.
     */
    public void doFile(String filename) {
        try {
            String NL = System.getProperty("line.separator");
            StringBuilder content = new StringBuilder();
            Scanner scanner;
            
            // Try to load the file fromt the file system.
            if (new File(filename).exists())
                scanner = new Scanner(new File(filename));
            
            // Otherwise, try from the Java file.
            else if (getClass().getResourceAsStream(filename) != null)
                scanner = new Scanner(getClass().getResourceAsStream(filename));
            else if (getClass().getResourceAsStream("/" + filename) != null)
                scanner = new Scanner(getClass().getResourceAsStream("/" + filename));
            
            // Otherwise, explode. :)
            else
                throw new FileNotFoundException(filename);

            while (scanner.hasNextLine()) {
                content.append(scanner.nextLine());
                content.append(NL);
            }

            doString(content.toString());
        } catch (FileNotFoundException e) {
            responses.add("Error: " + filename + " does not exist.");
        }
    }

    /**
     * Load a file containing Scheme code (bypasses "(load ...)")
     *
     * @param filename The filename.
     * @return Whatever value the last expression returns.
     */
    public String doFileAndWait(String filename) {
        doFile(filename);
        while (!hasResponse())
            try {
                Thread.sleep(50);
            } catch (InterruptedException ie) {
            }
        return nextResponse();
    }

    /**
     * Check if there any responses..
     *
     * @return True/false
     */
    public boolean hasResponse() {
        return !responses.isEmpty();
    }

    /**
     * Get the next response or null if there are no more (use hasResponses)
     *
     * @return The next response.
     */
    public String nextResponse() {
        if (hasResponse())
            return responses.remove();
        else
            return null;
    }
}
