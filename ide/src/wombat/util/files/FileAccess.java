/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.util.files;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.Scanner;

/**
 * Get files from either the operating system or the JAR files.
 */
public class FileAccess {
	static FileAccess me = new FileAccess();
	
	private FileAccess() {}

	/**
	 * Get a file by name.
	 * @param filename The filename to find.
	 * @return The content of that file.
	 * @throws FileNotFoundException If we cannot find it.
	 */
	public static String getFile(String filename) throws FileNotFoundException {
		return getFile(filename, false);
	}
	
	/**
	 * Get a file, skipping any local content (if flagged).
	 * @param filename The filename to find.
	 * @param skipLocal True to skip any local files.
	 * @return The content of that file.
	 * @throws FileNotFoundException If we cannot find it.
	 */
	public static String getFile(String filename, boolean skipLocal) throws FileNotFoundException {
		Scanner s;
		
		// Try to load the file from the file system.
	    if (!skipLocal && new File(filename).exists())
	    	s = new Scanner(new File(filename));
		else if (me.getClass().getResourceAsStream(filename) != null)
	    	s = new Scanner(new InputStreamReader(me.getClass().getResourceAsStream(filename)));
	    else if (me.getClass().getResourceAsStream("/" + filename) != null)
	    	s = new Scanner(new InputStreamReader(me.getClass().getResourceAsStream("/" + filename)));
	    
	    // Otherwise, no idea.
	    else
	    	throw new FileNotFoundException(filename);
	    
	    // Load the file.
	    StringBuilder contents = new StringBuilder();
	    while (s.hasNextLine())
	    {
	    	contents.append(s.nextLine());
	    	contents.append("\n");
	    }
	    return contents.toString();
	}
	
	/**
	 * Get the extension of a file.
	 * @param path The path to deal with.
	 * @return The extension.
	 */
	public static String extension(String path) {
		return path.substring(path.lastIndexOf(".") + 1);
	}
}
