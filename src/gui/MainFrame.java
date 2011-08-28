package gui;

import icons.IconManager;

import javax.swing.*;
import javax.swing.text.BadLocationException;

import wombat.Wombat;
import util.KawaWrap;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import net.infonode.docking.*;
import net.infonode.docking.util.*;

/**
 * Create a main frame.
 */
public class MainFrame extends JFrame {
	private static final long serialVersionUID = 2574330949324570164L;

	// Display components.
	RootWindow Root;
    KawaWrap Kawa;
    JToolBar ToolBar;
    
    // Unique code components.
    NonEditableTextArea History;
    NonEditableTextArea Output;
    NonEditableTextArea Error;
    NonEditableTextArea Debug;
    REPLTextArea REPL;

    /**
     * Don't directly create this, use me().
     * Use this method to set it up though.
     */
    public MainFrame() {
        // Set frame options.
        setTitle("Wombat - Build " + Wombat.VERSION);
        setSize(Options.DisplayWidth, Options.DisplayHeight);
        setLocation(Options.DisplayLeft, Options.DisplayTop);
        setLayout(new BorderLayout(5, 5));
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        try {
        	setIconImage(IconManager.icon("Wombat.png").getImage());
        } catch(NullPointerException ex) {
        	
        }
        
        // Wait for the program to end.
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
            	DocumentManager.CloseAll();
            	Options.DisplayTop = Math.max(0, e.getWindow().getLocation().y);
            	Options.DisplayLeft = Math.max(0, e.getWindow().getLocation().x);
            	Options.DisplayWidth = Math.max(400, e.getWindow().getWidth());
            	Options.DisplayHeight = Math.max(400, e.getWindow().getHeight());
            	Options.save();
            }
        });

        // Set up the menus using the above definitions.
        MenuManager.init(this);
        setJMenuBar(MenuManager.getMenu());
        
        // Create a display for any open documents.
        Root = DockingUtil.createRootWindow(new ViewMap(), true);
        TabWindow documents = new TabWindow();
        StringViewMap viewMap = new StringViewMap();
        DocumentManager.init(this, Root, viewMap, documents);
        DocumentManager.New();
         
        // Create displays for a split REPL.
        History = new NonEditableTextArea(this);
        Error = new NonEditableTextArea(this);
        Debug = new NonEditableTextArea(this);
        REPL = new REPLTextArea(this);
        viewMap.addView("REPL - Execute", new View("REPL - Execute", null, REPL));
        viewMap.addView("REPL - History", new View("REPL - History", null, History));
        SplitWindow replSplit = new SplitWindow(false, viewMap.getView("REPL - Execute"), viewMap.getView("REPL - History"));
        
        viewMap.getView("REPL - Execute").getWindowProperties().setCloseEnabled(false);
        viewMap.getView("REPL - History").getWindowProperties().setCloseEnabled(false);
        
        // Put everything together into the actual dockable display.
        SplitWindow fullSplit = new SplitWindow(false, 0.6f, documents, replSplit);
        Root.setWindow(fullSplit);
        add(Root);
        
        // Connect to Kawa.
        Kawa = new KawaWrap();
        Kawa.eval("(current-output-port (util.SchemePrinter:new (*:.Output (gui.MainFrame))))");
		Kawa.eval("(current-error-port (util.SchemePrinter:new (*:.Error (gui.MainFrame))))");
        
        // Add a toolbar.
        ToolBar = new JToolBar();
        ToolBar.setFloatable(false);
        for (Action a : new Action[]{new actions.New(), new actions.Open(), new actions.Save(), new actions.Close()})
        	ToolBar.add(a);
        ToolBar.addSeparator();
        for (Action a : new Action[]{new actions.Run(), new actions.Format(), new actions.Reset()})
        	ToolBar.add(a);
        
        add(ToolBar, BorderLayout.PAGE_START);
        ToolBar.setVisible(Options.DisplayToolbar);
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

        History.append("\n§ " + command.replace("\n", "\n  ") + "\n");
        
        Object result = Kawa.eval(command);
        if (result != null)
        	History.append(result.toString() + "\n");
    }

    /**
     * Update the display.
     */
	public boolean updateDisplay() {
		boolean reloaded = true;
		for (SchemeTextArea ss : new SchemeTextArea[]{History, Output, Error, Debug, REPL}) {
			try {
				((SchemeDocument) ss.code.getDocument()).processChangedLines(0, ss.getText().length());
	    	}  catch (BadLocationException e) {
	    		reloaded = false;
				ErrorFrame.log("Unable to format History view: " + e.getMessage());
			}
		}
		return reloaded;
	}

	/**
	 * Focus the REPL.
	 */
	public void focusREPL() {
		REPL.requestFocusInWindow();
	}

	/**
	 * Set the toolbar's display mode.
	 * @param displayToolbar If the toolbar should be visible.
	 */
	public void toggleToolbar(boolean displayToolbar) {
		ToolBar.setVisible(displayToolbar);
	}

	/**
	 * Reset Kawa.
	 */
	public void resetKawa() {
		Kawa.reset();
		History.append("\n>>> Environment reset <<<\n");
	}
}
