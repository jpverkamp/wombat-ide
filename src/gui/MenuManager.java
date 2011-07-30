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
import net.infonode.docking.View;

/**
 * Handle all of the code for building and maintaining menus.
 */
public class MenuManager implements ActionListener {
    static MenuManager me;

    JFileChooser fileDialog;

    JMenuBar menu;
    
    Map<String, JMenuItem> nameToItem;
    Map<JMenuItem, String> itemToName;

    /**
     * Build the main menu.
     */
    private MenuManager() {
        nameToItem = new HashMap<String, JMenuItem>();
        itemToName = new HashMap<JMenuItem, String>();

        fileDialog = new JFileChooser();
        
        menu = buildBar(
            buildMenu("File",
                buildItem("New", 'N'),
                buildItem("Open", 'O'),
                buildItem("Save", 'S'),
                buildItem("Save as", null),
                buildItem("Close", 'W'),
                buildItem("Exit", null)),
            buildMenu("Scheme",
                buildItem("Run", Options.get("scheme.run", "F5")),
                buildItem("Format", Options.get("scheme.run", "F6"))),
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
                    (Character) accel,
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

       return me.menu;
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
    }

    /**
     * Run a command.
     *
     * @param cmd The command to run.
     */
    private boolean doCommand(String cmd) {
        DocumentManager dm = MainFrame.me().Documents;

        // Create a new file in a new tab.
        if ("New".equals(cmd)) {
            dm.New();
        }

        // Load a file into a new tab.
        else if ("Open".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (JFileChooser.APPROVE_OPTION == fileDialog.showOpenDialog(MainFrame.me()))
                return dm.Load(fileDialog.getSelectedFile());

            return false;
        }

        // Save the currently active file.
        else if ("Save".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (v == null)
                return false;

            if (!dm.HasFile(v))
                return doCommand("Save as");
            else
                return dm.Save(v);
        }

        // Save the currently selected file with a new name().
        else if ("Save as".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (v == null)
                return false;

            if (JFileChooser.APPROVE_OPTION == fileDialog.showSaveDialog(MainFrame.me()))
            {
                dm.SetFile(v, fileDialog.getSelectedFile());
                return dm.Save(v);
            }
            else
                return false;
        }

        // Close the active document (save if it had content).
        else if ("Close".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (v == null)
                return false;

            if (!dm.IsEmpty(v))
                if (!doCommand("Save"))
                    return false;

            if (dm.Documents.getChildWindowCount() == 1)
                doCommand("New");

            v.close();

            return true;
        }

        // Exit the program.
        else if ("Exit".equals(cmd)) {
            MainFrame.me().dispose();
            return true;
        }

        // Run the currently selected file.
        else if ("Run".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (v == null)
                return false;

            if (!dm.HasFile(v))
            {
                if (doCommand("Save as"))
                    doCommand("Run");
            }
            else
            {
                MainFrame.me().doCommand("(load \"" + dm.GetFile(v).getAbsolutePath().replace("\\", "/")  + "\")");
                MainFrame.me().REPL.code.requestFocusInWindow();
                return true;
            }

            return false;
        }

        // Format the current code.
        else if ("Format".equals(cmd)) {
            View v = MainFrame.me().Root.getFocusedView();

            if (v == null)
                return false;

            SchemeTextArea ss = (SchemeTextArea) v.getComponent();
            ss.format();
            return true;
        }

        // Load the configuration file to edit.
        else if ("Edit configuration".equals(cmd)) {
            File f = new File(Options.FILENAME);
            if (!f.exists())
                exportFromJar(Options.FILENAME);
            return dm.Load(f);
        }

        // Load the syntax file to edit.
        else if ("Edit syntax highlighting".equals(cmd)) {
            File f = new File(SchemeDocument.FILENAME);
            if (!f.exists())
                exportFromJar(SchemeDocument.FILENAME);
            return dm.Load(f);
        }

        // Reload options.
        else if ("Reload options".equals(cmd)) {
            Options.reload();
            SchemeDocument.reload();
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
