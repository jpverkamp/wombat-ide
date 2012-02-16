package wombat.util.files;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.Scanner;

public class FileAccess {
	static FileAccess me = new FileAccess();
	
	private FileAccess() {}
	
	public static String getFile(String filename) throws FileNotFoundException {
		return getFile(filename, false);
	}
	
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
	
	public static String extension(String path) {
		return path.substring(path.lastIndexOf(".") + 1);
	}
}
