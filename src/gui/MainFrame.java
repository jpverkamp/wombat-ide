package gui;

import scheme.ForeignScheme;
import scheme.SISCScheme;
import scheme.Scheme;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.FileNotFoundException;
import java.util.Stack;

/**
 * Create a main frame.
 */
public class MainFrame extends JFrame {
    // Woo singletons.
    static MainFrame me;

    // Things we may need access to.
    JTabbedPane Tabs;
    SchemeTextArea History;
    SchemeTextArea REPL;
    Scheme SS;

    /**
     * Don't directly create this, use me().
     * Use this method to set it up though.
     */
    private MainFrame() {
        // Set frame options.
        setTitle("Wombat");
        try {
            setSize(
                    Integer.parseInt(Options.get("main.width")),
                    Integer.parseInt(Options.get("main.height"))
            );
        } catch (Exception e) {
            setSize(600, 400);
        }
        setLayout(new BorderLayout(5, 5));
        setLocationByPlatform(true);
        // Wait for the program to end.
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });

        // Define the menu options.
        String[][][] menus = {
                {{"File", null},
                        {"New", "control N"},
                        {"Open", "control O"},
                        {"Save", "control S"},
                        {"Save as", null},
                        {"Close", "control W"},
                        {"Exit", "alt F4"}},
                {{"Scheme", null},
                        {"Run", Options.get("scheme.run", "F5")},
                        {"Format", Options.get("scheme.format", "F6")}},
                /*
                {{"Options", null},
                        {"Edit configuration", null},
                        {"Edit syntax highlighting", null}},
                 */
                {{"Help", null},
                        {"About", "F1"}}
        };

        // Set up the menus using the above definitions.
        JMenuBar menuBar = new JMenuBar();
        for (int i = 0; i < menus.length; i++) {
            JMenu menu = new JMenu(menus[i][0][0]);
            for (int j = 1; j < menus[i].length; j++) {
                JMenuItem item = new JMenuItem(menus[i][j][0]);
                if (menus[i][j][1] != null) item.setAccelerator(KeyStroke.getKeyStroke(menus[i][j][1]));
                item.addActionListener(MenuListener.me());
                menu.add(item);
            }
            menuBar.add(menu);
        }
        setJMenuBar(menuBar);

        // Set up components.
        Tabs = new JTabbedPane();
        Tabs.addTab("<new document>", new SchemeTextArea());
        Tabs.setPreferredSize(new Dimension(100, getHeight() / 2));
        add(Tabs, BorderLayout.CENTER);

        JPanel bottom = new JPanel();
        bottom.setLayout(new BorderLayout());
        add(bottom, BorderLayout.SOUTH);

        History = new SchemeTextArea();
        History.setPreferredSize(new Dimension(100, getHeight() / 2 - 100));
        History.code.setEditable(false);
        bottom.add(History, BorderLayout.CENTER);

        REPL = new SchemeTextArea();
        REPL.setPreferredSize(new Dimension(100, 100));
        bottom.add(REPL, BorderLayout.SOUTH);
        REPL.code.getInputMap().put(
                KeyStroke.getKeyStroke("ENTER"),
                new AbstractAction() {
                    public void actionPerformed(ActionEvent e) {
                        Stack<Character> brackets = new Stack<Character>();
                        for (char c : REPL.getText().toCharArray()) {
                            if (c == '(') brackets.push(')');
                            else if (c == '[') brackets.push(']');
                            else if (c == ')' || c == ']')
                                if (!brackets.empty() && brackets.peek() == c)
                                    brackets.pop();
                                else
                                    return;
                        }

                        if (brackets.empty()) {
                            doCommand(REPL.getText());
                            REPL.setText("");
                        } else {
                            try {
                                REPL.code.getDocument().insertString(REPL.code.getCaretPosition(), SchemeTextArea.NL, null);
                            } catch (BadLocationException ble) {
                                System.err.println("badwolf");
                            }
                            REPL.tab();
                        }
                    }
                }
        );

        // Get a Scheme.
        String schemeVersion = Options.get("scheme", "sisc");
        if ("sisc".equals(schemeVersion.toLowerCase())) {
            SS = new SISCScheme();
        } else {
            try {
                SS = new ForeignScheme(schemeVersion);
            } catch (FileNotFoundException e) {
                History.append("Scheme not found: " + schemeVersion + "\n");
                History.append("Falling back to SISC.\n\n");
                SS = new SISCScheme();
            }
        }

        // Bind it to return responses to the History pane.
        Thread t = new Thread(new Runnable() {
            public void run() {
                while (true) {
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException ie) {
                    }

                    if (SS.hasResponse())
                        History.append(SS.nextResponse() + "\n");
                }
            }
        });
        t.setDaemon(true);
        t.start();
    }

    /**
     * Run a command.
     *
     * @param command The command to run.
     */
    void doCommand(String command) {
        command = command.trim();
        if (command.length() == 0)
            return;

        History.append("\n>>> " + command.replace("\n", "\n    ") + "\n");
        SS.doString(command);
    }

    /**
     * Access the frame.
     *
     * @return The singleton frame.
     */
    public static MainFrame me() {
        if (me == null)
            me = new MainFrame();

        return me;
    }
}
