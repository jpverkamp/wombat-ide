package wombat.util.errorlog;

import java.io.ByteArrayOutputStream; 
import java.io.IOException; 
import java.util.logging.Level; 
import java.util.logging.Logger; 
 
/**
 * LICENSE:
 * https://blogs.oracle.com/nickstephen/entry/java_redirecting_system_out_and
 * 
 * The code in this article is free for any use. I'd appreciate having a back-reference
 * URL to the blog article in a comment in the code, but that's all. Thanks for the question! [ Nick ]
 */

/** 
 * An OutputStream that writes contents to a Logger upon each call to flush() 
 */ 
public class LoggingOutputStream extends ByteArrayOutputStream { 
 
    private String lineSeparator; 
 
    private Logger logger; 
    private Level level; 
 
    /** 
     * Constructor 
     * @param logger Logger to write to 
     * @param level Level at which to write the log message 
     */ 
    public LoggingOutputStream(Logger logger, Level level) { 
        super(); 
        this.logger = logger; 
        this.level = level; 
        lineSeparator = System.getProperty("line.separator"); 
    } 
 
    /** 
     * upon flush() write the existing contents of the OutputStream
     * to the logger as a log record. 
     * @throws java.io.IOException in case of error 
     */ 
    public void flush() throws IOException { 
 
        String record; 
        synchronized(this) { 
            super.flush(); 
            record = this.toString(); 
            super.reset(); 
 
            if (record.length() == 0 || record.equals(lineSeparator)) { 
                // avoid empty records 
                return; 
            } 
 
            logger.logp(level, "", "", record); 
        } 
    } 
} 