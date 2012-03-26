/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.frames;

import java.awt.Toolkit;
import java.util.HashMap;
import java.util.Map;

import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

import wombat.util.Options;
import wombat.util.files.RecentDocumentManager;

/**
 * Handle all of the code for building and maintaining menus.
 */
public final class MenuManager {
	static MenuManager me;
	
    JFileChooser fileDialog;
    JMenuBar myMenu;
    
    public Map<String, JMenuItem> nameToItem;
    public Map<JMenuItem, String> itemToName;

    /**
     * Build the menu.
     * @param main The main frame.
     */
    private MenuManager(MainFrame main) {
    	// Maps to and from items and their names.
    	nameToItem = new HashMap<String, JMenuItem>();
        itemToName = new HashMap<JMenuItem, String>();

        // The file chooser.
        fileDialog = new JFileChooser();
        
        // Actually build the menu based on all of the helper methods we have.
        // Nulls will turn into menu seperators.
        myMenu = buildBar(
        		
    		// Opens for interacting with files.
            buildMenu("File", 'F',
                buildItem("New", 'N', new wombat.gui.actions.New()),
                RecentDocumentManager.buildRecentDocumentsMenu(),
                buildItem("Open", 'O', new wombat.gui.actions.Open()),
                buildItem("Save", 'S', new wombat.gui.actions.Save()),
                buildItem("Save as", null, new wombat.gui.actions.SaveAs()),
                buildItem("Close", 'W', new wombat.gui.actions.Close()),
                null,
                buildItem("Connect", null, new wombat.gui.actions.Connect()),
                buildItem("Upload", null, new wombat.gui.actions.Upload()),
                null,
                buildItem("Exit", "ALT F4", new wombat.gui.actions.Exit())),
                
            // Options for interacting with the current document.
            buildMenu("Edit",
        		buildItem("Cut", 'X', new wombat.gui.actions.Cut()),
        		buildItem("Copy", 'C', new wombat.gui.actions.Copy()),
        		buildItem("Paste", 'V', new wombat.gui.actions.Paste()),
        		null,
        		buildItem("Undo", 'Z', new wombat.gui.actions.Undo()),
        		buildItem("Redo", 'Y', new wombat.gui.actions.Redo()),
        		null,
        		buildItem("Find/Replace", 'F', new wombat.gui.actions.FindReplace())),
        		
        	// Options for interacting with the embedded scheme system or formatting scheme code.
            buildMenu("Scheme",
                buildItem("Run", Options.CommandRun, new wombat.gui.actions.Run()),
                buildItem("Stop", null, new wombat.gui.actions.Stop()),
                buildItem("Format", Options.CommandFormat, new wombat.gui.actions.Format()),
                buildItem("Reset", null, new wombat.gui.actions.Reset())),
                
            // Actual options, based on which options we're going to use.
            Options.buildOptionsMenu(main),
            
            // Help menu, mostly for debugging and about.
            buildMenu("Help",
                buildItem("Show debug console", null, new wombat.gui.actions.ShowError()),
                buildItem("About", "F1", new wombat.gui.actions.ShowAbout())));
    }
    
    /**
     * Build the main menu.
     * @param main The menu manager.
     */
    public static MenuManager Singleton(MainFrame main) {
    	if (me == null) me = new MenuManager(main);
    	return me;
    }
    
    /**
     * Get the menu.
     * @return The menu.
     */
    public static JMenuBar getMenu() {
    	return me.myMenu;
    }
    
    /**
     * Get the menu item associated with a name.
     * @param name The menu item's name.
     * @return The menu item.
     */
    public static JMenuItem itemForName(String name) {
    	return me.nameToItem.get(name);
    }
    
    /**
     * Get the name associated with a menu item.
     * @param item The menu item.
     * @return It's name.
     */
    public static String nameForItem(JMenuItem item) {
    	return me.itemToName.get(item);
    }
    
    /**
     * Build a JMenuBar.
     * @param menus All of the menus.
     * @return A menu bar based on a number of submenus. 
     */
    private JMenuBar buildBar(JMenu... menus) {
    	JMenuBar menuBar = new JMenuBar();
        for (JMenu menu : menus)
            menuBar.add(menu);
        return menuBar;
    }

    /**
     * Build a menu with a mnemonic.
     * @param name The menu's name.
     * @param accel The mnemonic.
     * @param items A list of JMenuItems.
     * @return The menu.
     */
    private JMenu buildMenu(String name, char accel, JMenuItem... items) {
        JMenu menu = buildMenu(name, items);
        if (!wombat.util.OS.IsOSX)
            menu.setMnemonic(accel);
        return menu;
    }

    /**
     * Build a menu.
     * @param name The menu's name.
     * @param items A list of JMenuItems.
     * @return The menu.
     */
    private JMenu buildMenu(String name, JMenuItem... items) {
        JMenu menu = new JMenu(name);
        for (JMenuItem item : items)
        	if (item == null)
        		menu.addSeparator();
        	else
            	menu.add(item);
        return menu;
    }
    
    /**
     * Build a JMenuItem (and store it in the dictionaries).
     * 
     * @param action An action.
     * @return The new item.
     */
	private JMenuItem buildItem(String name, Object accel, Action action) {
    	JMenuItem item = new JMenuItem(action);
    	item.setText(name);
    	
    	// If an accelerator was specified, use it (and also as the mnemonic on non-osx).
        if (accel != null) {
            // Something like 'C' -> Ctrl-C / Cmd-C on OSX
        	if (accel instanceof Character)
                item.setAccelerator(KeyStroke.getKeyStroke(
                    ((Character) accel).charValue(),
                    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
                ));

        	// Something like "Ctrl C" -> Ctrl-C
            else if (accel instanceof String)
                item.setAccelerator(KeyStroke.getKeyStroke((String) accel));

        	// Only set mnemonics when not on OSX.
            if (!wombat.util.OS.IsOSX && accel instanceof Character)
                item.setMnemonic((Character) accel);
        }
    	
        // Add to maps.
    	itemToName.put(item, name);
    	nameToItem.put(name, item);
    	
    	return item;
    }
}
