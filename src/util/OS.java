package util;

/**
 * Determine OS..
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
}
