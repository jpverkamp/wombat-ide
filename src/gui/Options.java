package gui;

import java.awt.Color;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.HashMap;
import java.util.Map;
import java.util.prefs.Preferences;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;

/**
 * Store options.
 */
public final class Options {
	static Preferences prefs;
	static JMenu optionsMenu;
			
	// Used for the main display. 
	public static int DisplayWidth = 600;
	public static int DisplayHeight = 800;
	public static int DisplayTop = 100;
	public static int DisplayLeft = 100;
	
	// Keyboard shortcuts.
	public static String CommandRun = "F5";
	public static String CommandFormat = "F6";
	
	// Options menu options.
	public static boolean EmacsKeybindings = false;
	public static boolean ConfirmOnRunClose = true;
	
	// Syntax highlighting.
	public static Map<String, Color> Colors;
	public static Map<String, Integer> Keywords;

	/**
     * Initialize.
     */
    static {
    	prefs = Preferences.userNodeForPackage(new Options().getClass());
    	
    	DisplayTop = prefs.getInt("Display/Top", DisplayTop);
    	DisplayLeft = prefs.getInt("Display/Left", DisplayLeft);
    	DisplayWidth = prefs.getInt("Display/Width", DisplayWidth);
    	DisplayHeight = prefs.getInt("Display/Height", DisplayHeight);
    	
    	CommandRun = prefs.get("Command/Run", CommandRun);
    	CommandFormat = prefs.get("Command/Format", CommandFormat);

    	EmacsKeybindings = prefs.getBoolean("Options/EmacsKeybindings", EmacsKeybindings);
    	ConfirmOnRunClose = prefs.getBoolean("Options/ConfirmOnRunClose", ConfirmOnRunClose);
    	
    	Colors = new HashMap<String, Color>();
    	Colors.put("normal", new Color(prefs.getInt("Colors/normal", 0x000000)));
    	Colors.put("comment", new Color(prefs.getInt("Colors/comment", 0x006600)));
    	Colors.put("keyword", new Color(prefs.getInt("Colors/keyword", 0x000099)));
    	Colors.put("string", new Color(prefs.getInt("Colors/string", 0xFF8C00)));
    	Colors.put("bracket", new Color(prefs.getInt("Colors/bracket", 0x00FFFF)));
    	
    	Keywords = new HashMap<String, Integer>();
    	
    	SchemeDocument.reload();
    }
    
    /**
     * Save to preferences on dispose.
     */
    public static void save() {
    	prefs.putInt("Display/Top", DisplayTop);
    	prefs.putInt("Display/Left", DisplayLeft);
    	prefs.putInt("Display/Width", DisplayWidth);
    	prefs.putInt("Display/Height", DisplayHeight);
    	
    	prefs.put("Command/Run", CommandRun);
    	prefs.put("Command/Format", CommandFormat);
    	
    	prefs.putBoolean("Options/EmacsKeybindings", EmacsKeybindings);
    	prefs.putBoolean("Options/ConfirmOnRunClose", ConfirmOnRunClose);
    	
    	for (String key : Colors.keySet())
    		prefs.putInt("Colors/" + key, Colors.get(key).getRGB());
    }
    
    /**
     * Build the options menu (only build once).
     * @return An options menu.
     */
    public static JMenu buildOptionsMenu() {
    	if (optionsMenu == null) {
    		optionsMenu = new JMenu("Options");
    		
    		JCheckBoxMenuItem emacs = new JCheckBoxMenuItem("Enable emacs keybindings", EmacsKeybindings);
    		emacs.addItemListener(new ItemListener() {
				public void itemStateChanged(ItemEvent arg0) {
					EmacsKeybindings = ((JCheckBoxMenuItem) arg0.getSource()).isSelected();
				}
    		});
    		optionsMenu.add(emacs);
    		
    		JCheckBoxMenuItem confirm = new JCheckBoxMenuItem("Confirm on Run/Close", ConfirmOnRunClose);
    		confirm.addItemListener(new ItemListener() {
				public void itemStateChanged(ItemEvent arg0) {
					ConfirmOnRunClose = ((JCheckBoxMenuItem) arg0.getSource()).isSelected();
				}
    		});
    		optionsMenu.add(confirm);
    		
    	}
    	
    	return optionsMenu;
    }
    
    // Hide this.
    private Options() {};
}
