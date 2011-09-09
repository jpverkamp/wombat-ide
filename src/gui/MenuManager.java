/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import javax.swing.*;
import javax.swing.text.DefaultEditorKit;

import util.files.RecentDocumentManager;
import wombat.Options;

import java.awt.Toolkit;
import java.util.*;

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
    	nameToItem = new HashMap<String, JMenuItem>();
        itemToName = new HashMap<JMenuItem, String>();

        fileDialog = new JFileChooser();
        
        myMenu = buildBar(
            buildMenu("File", 'F',
                    buildItem("New", 'N', new actions.New()),
                    RecentDocumentManager.buildRecentDocumentsMenu(),
                    buildItem("Open", 'O', new actions.Open()),
                    buildItem("Save", 'S', new actions.Save()),
                    buildItem("Save as", null, new actions.SaveAs()),
                    buildItem("Close", 'W', new actions.Close()),
                    buildItem("Exit", "ALT F4", new actions.Exit())),
                buildMenu("Edit",
            		buildItem("Cut", 'X', new DefaultEditorKit.CutAction()),
            		buildItem("Copy", 'C', new DefaultEditorKit.CopyAction()),
            		buildItem("Paste", 'V', new DefaultEditorKit.PasteAction())),
                buildMenu("Scheme",
                    buildItem("Run", Options.CommandRun, new actions.Run()),
                    buildItem("Stop", null, new actions.Stop()),
                    buildItem("Format", Options.CommandFormat, new actions.Format()),
                    buildItem("Reset", null, new actions.Reset())),
                Options.buildOptionsMenu(main),
                buildMenu("Help",
                    buildItem("Show debug console", null, new actions.ShowError()),
                    buildItem("About", "F1", new actions.ShowAbout())));
    }
    
    /**
     * Build the main menu.
     * @param main 
     */
    public static MenuManager init(MainFrame main) {
    	if (me != null) throw new RuntimeException("Attempted to initialize MenuManager more than once.");
    	
    	me = new MenuManager(main);
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
     * @return 
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
     * @return
     */
    private JMenu buildMenu(String name, char accel, JMenuItem... items) {
        JMenu menu = buildMenu(name, items);
        if (!util.OS.IsOSX)
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
    	
        if (accel != null)
        {
            if (accel instanceof Character)
                item.setAccelerator(KeyStroke.getKeyStroke(
                    ((Character) accel).charValue(),
                    Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()
                ));

            else if (accel instanceof String)
                item.setAccelerator(KeyStroke.getKeyStroke((String) accel));

            if (!util.OS.IsOSX && accel instanceof Character)
                item.setMnemonic((Character) accel);
        }
    	
    	itemToName.put(item, name);
    	nameToItem.put(name, item);
    	
    	return item;
    }
}
