/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import javax.swing.*;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.*;

/**
 * Handle all of the code for building and maintaining menus.
 */
public final class MenuManager implements ActionListener {
    static MenuManager me;

    JFileChooser fileDialog;

    JMenuBar myMenu;
    
    Map<String, JMenuItem> nameToItem;
    Map<JMenuItem, String> itemToName;

    /**
     * Build the main menu.
     */
    private MenuManager() {
        nameToItem = new HashMap<String, JMenuItem>();
        itemToName = new HashMap<JMenuItem, String>();

        fileDialog = new JFileChooser();
        
        myMenu = buildBar(
            buildMenu("File", 'F',
                buildItem("New", 'N'),
                buildItem("Open", 'O'),
                buildItem("Save", 'S'),
                buildItem("Save as", null),
                buildItem("Close", 'W'),
                buildItem("Exit", "ALT F4")),
            buildMenu("Scheme",
                buildItem("Run", Options.get("scheme.run", "F5")),
                buildItem("Format", Options.get("scheme.format", "F6"))),
            buildMenu("Options",
                buildItem("Edit configuration", null),
                buildItem("Edit syntax highlighting", null),
                buildItem("Reload options", null)),
            buildMenu("Help",
                buildItem("Show error console", null),
                buildItem("About", "F1")));
    }

    /**
     * Build a JMenuBar.
     * @param menus All of the menus.
     * @return 
     */
    public JMenuBar buildBar(JMenu... menus)
    {
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
    private JMenu buildMenu(String name, char accel, JMenuItem... items)
    {
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
    private JMenu buildMenu(String name, JMenuItem... items)
    {
        JMenu menu = new JMenu(name);
        for (JMenuItem item : items)
            menu.add(item);
        return menu;
    }

    /**
     * Build a JMenuItem (and store it in the dictionaries).
     *
     * @param name Text for the item (can be changed later with i18n)
     * @param accel If a character use the system shortcut key. If a string, use
     *              the getKeyStroke method to decode it.
     * @return The new item.
     */
    private JMenuItem buildItem(String name, Object accel)
    {
        JMenuItem item = new JMenuItem(name);
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

        item.addActionListener(this);

        return item;
    }

    /**
     * Access the JMenuBar.
     * @return Said JMenuBar.
     */
    public static JMenuBar menu() {
       if (me == null)
           me = new MenuManager();

       return me.myMenu;
    }

    /**
     * When the menu item is clicked. Dispatch to doCommand based on the menu item name().
     *
     * @param e The event.
     */
    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() instanceof JMenuItem)
            doCommand(itemToName.get((JMenuItem) e.getSource()));
        else
            ErrorFrame.log("Non-menu using menu action listener: " + e.getSource());
    }

    /**
     * Run a command.
     *
     * @param cmd The command to run.
     */
    private boolean doCommand(String cmd) {
        DocumentManager dm = MainFrame.me().Documents;

        // File options, defer to the document manager.
        if ("New".equals(cmd))
            return dm.New();
        else if ("Open".equals(cmd))
            return dm.Open();
        else if ("Save".equals(cmd))
            return dm.Save();
        else if ("Save as".equals(cmd))
            return dm.SaveAs();
        else if ("Close".equals(cmd))
            return dm.Close();
        else if ("Run".equals(cmd))
            dm.Run();
        else if ("Format".equals(cmd))
            dm.Format();

        // Exit the program.
        else if ("Exit".equals(cmd)) {
            MainFrame.me().dispose();
            return true;
        }

        // Load the configuration file to edit.
        else if ("Edit configuration".equals(cmd)) {
            File f = new File(Options.OPTIONS_FILE);
            if (!f.exists())
                exportFromJar(Options.OPTIONS_FILE);
            return dm.Open(f);
        }

        // Load the syntax file to edit.
        else if ("Edit syntax highlighting".equals(cmd)) {
            File f = new File(Options.SYNTAX_FILE);
            if (!f.exists())
                exportFromJar(Options.SYNTAX_FILE);
            return dm.Open(f);
        }

        // Reload options.
        else if ("Reload options".equals(cmd)) {
            Options.reload();
        }

        // Show the error dialog.
        else if ("Show error console".equals(cmd))
        {
            ErrorFrame.showMe();
            return true;
        }

        // Show the about dialog.
        else if ("About".equals(cmd)) {
            AboutFrame.showMe();
            return true;
        }

        // Explode. Horribly.
        else {
            ErrorFrame.log("Unknown menu item selected: " + cmd);
        }

        return false;
    }

    /**
     * Grab a file from the JAR and make a real file out of it.
     * @param res
     */
    private void exportFromJar(String res)
    {
        try
        {
            // Get the input stream from the JAR.
            InputStream fromJar;
            if ((fromJar = getClass().getResourceAsStream(res)) == null)
                if ((fromJar = getClass().getResourceAsStream("/" + res)) == null)
                    throw new FileNotFoundException(res);

            // Get the output stream to the file.
            OutputStream toFile = new FileOutputStream(new File(res));

            // Copy.
            byte[] buf = new byte[8192];
            while (true) {
              int length = fromJar.read(buf);
              if (length < 0)
                break;
              toFile.write(buf, 0, length);
            }

            // Close buffers.
            fromJar.close();
            toFile.close();
        }
        catch (FileNotFoundException ex)
        {
            ex.printStackTrace();
        }
        catch (IOException ex)
        {
            ex.printStackTrace();
        }

    }
}
