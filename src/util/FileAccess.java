package util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;

public class FileAccess {
	static FileAccess me = new FileAccess();
	
	private FileAccess() {}
	
	public static Reader getFile(String filename) throws FileNotFoundException
	{
		// Try to load the file from the file system.
	    if (new File(filename).exists())
	    	return new FileReader(new File(filename));
		else if (me.getClass().getResourceAsStream(filename) != null)
	    	return new InputStreamReader(me.getClass().getResourceAsStream(filename));
	    else if (me.getClass().getResourceAsStream("/" + filename) != null)
	    	return new InputStreamReader(me.getClass().getResourceAsStream("/" + filename));
	    
	    // Otherwise, no idea.
	    else
	    	throw new FileNotFoundException(filename);
	}
}
