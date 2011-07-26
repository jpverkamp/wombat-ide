package gui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.Scanner;

/**
 * Listen for menu actions.
 */
class MenuListener implements ActionListener {
    static MenuListener me;
    JFileChooser FileDialog;

    /**
     * Initialize the menu listener.
     */
    private MenuListener() {
        FileDialog = new JFileChooser();
    }

    /**
     * Access the menu listener.
     *
     * @return The menu listener.
     */
    public static MenuListener me() {
        if (me == null)
            me = new MenuListener();

        return me;
    }

    /**
     * When the menu item is clicked. Dispatch to doCommand based on the menu item name.
     *
     * @param e The event.
     */
    public void actionPerformed(ActionEvent e) {
        doCommand(e.getActionCommand());
    }

    /**
     * Run a command.
     *
     * @param cmd The command to run.
     */
    private boolean doCommand(String cmd) {
        SchemeTextArea activeTab = (SchemeTextArea) MainFrame.me().Tabs.getSelectedComponent();

        if ("New".equals(cmd)) {
            MainFrame.me().Tabs.addTab("<new document>", new SchemeTextArea());
            MainFrame.me().Tabs.setSelectedIndex(MainFrame.me().Tabs.getTabCount() - 1);
            ((SchemeTextArea) MainFrame.me().Tabs.getSelectedComponent()).code.requestFocusInWindow();
            return true;
        } else if ("Open".equals(cmd)) {
            if (JFileChooser.APPROVE_OPTION == FileDialog.showOpenDialog(MainFrame.me()))
                return loadFile(FileDialog.getSelectedFile());
        } else if ("Save".equals(cmd)) {
            if (activeTab.getFile() == null)
                if (!doCommand("Save as"))
                    return false;

            try {
                Writer out = new OutputStreamWriter(new FileOutputStream(activeTab.getFile()));
                out.write(activeTab.getText());
                out.flush();
                out.close();
                return true;
            } catch (IOException e2) {
                System.err.println(e2.getMessage());
                return false;
            }
        } else if ("Save as".equals(cmd)) {
            if (JFileChooser.APPROVE_OPTION == FileDialog.showSaveDialog(MainFrame.me())) {
                MainFrame.me().Tabs.setTitleAt(
                        MainFrame.me().Tabs.getSelectedIndex(),
                        FileDialog.getSelectedFile().getName()
                );
                activeTab.setFile(FileDialog.getSelectedFile());

                return doCommand("Save");
            }

            return false;
        } else if ("Close".equals(cmd)) {
            if (activeTab.getText().length() != 0)
                if (!doCommand("Save"))
                    return false;
            
            MainFrame.me().Tabs.removeTabAt(MainFrame.me().Tabs.getSelectedIndex());
            if (MainFrame.me().Tabs.getTabCount() == 0)
                doCommand("New");

            return true;
        } else if ("Exit".equals(cmd)) {
            MainFrame.me().dispose();
            return true;
        } else if ("Run".equals(cmd)) {
            if (!doCommand("Save"))
                return false;

            MainFrame.me().doCommand("(load \"" + activeTab.getFile().getPath().replace("\\", "/") + "\")");
            MainFrame.me().REPL.code.requestFocusInWindow();
        } else if ("Format".equals(cmd)) {
            activeTab.format();
            return true;
        } else if ("Edit configuration".equals(cmd)) {
            return loadFile(new File("options.cfg"));
        } else if ("Edit syntax highlighting".equals(cmd)) {
            return loadFile(new File("syntax.cfg"));
        } else if ("About".equals(cmd)) {
            // TODO: Fix this
        } else {
            // TODO: Fix this
        }

        return false;
    }

    /**
     * Load a file.
     *
     * @param file The file to load.
     * @return If the file loaded successful.
     */
    private boolean loadFile(File file) {
        try {
            SchemeTextArea activeTab = (SchemeTextArea) MainFrame.me().Tabs.getSelectedComponent();

            if (!file.exists())
                file.createNewFile();

            // Get the new content.
            Scanner scanner = new Scanner(file);
            StringBuilder content = new StringBuilder();
            String NL = System.getProperty("line.separator");

            while (scanner.hasNextLine()) {
                content.append(scanner.nextLine());
                content.append(NL);
            }

            // Create a new tab.
            MainFrame.me().Tabs.addTab(
                    file.getName(),
                    new SchemeTextArea(content.toString())
            );
            MainFrame.me().Tabs.setSelectedIndex(MainFrame.me().Tabs.getTabCount() - 1);
            activeTab = (SchemeTextArea) MainFrame.me().Tabs.getSelectedComponent();
            activeTab.setFile(file);
            activeTab.code.requestFocusInWindow();
            return true;
        } catch (FileNotFoundException e2) {
            System.err.println(e2.getMessage());
            return false;
        } catch (IOException e2) {
            System.err.println(e2.getMessage());
            return false;
        }
    }
}
