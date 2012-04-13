/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.util;

/**
 * Determine which operating system we're running on.
 */
public class OS {
    private OS() {}

    private final static String os = System.getProperty("os.name").toLowerCase();

    /**
     * True if the OS is some flavor of Windows.
     */
    public final static boolean IsWindows = os.indexOf("win") != -1;

    /**
     * True if the OS is some flavor of OSX.
     */
    public final static boolean IsOSX = os.indexOf("mac") != -1;

    /**
     * True if the OS is some flavor of unix/linux (hopefully).
     */
    public final static boolean IsLinux = (os.indexOf("nux") != -1) || (os.indexOf("nix") != -1);
    
    /**
     * True on 64-bit systems. Theoretically.
     */
    public static final boolean Is64Bit = System.getProperty("os.arch").indexOf("64") != -1; 
}
