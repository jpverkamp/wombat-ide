package gui;

import gnu.mapping.OutPort;
import icons.IconManager;

import javax.swing.*;
import javax.swing.text.BadLocationException;

import wombat.Wombat;
import util.KawaWrap;
import util.errors.ErrorListener;
import util.errors.ErrorManager;

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
    StringViewMap ViewMap;
    
    // Unique code components.
    NonEditableTextArea History;
    NonEditableTextArea Display;
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
        ViewMap = new StringViewMap();
        DocumentManager.init(this, Root, ViewMap, documents);
        DocumentManager.New();
         
        // Create displays for a split REPL.
        History = new NonEditableTextArea(this);
        REPL = new REPLTextArea(this);
        ViewMap.addView("REPL - Execute", new View("REPL - Execute", null, REPL));
        ViewMap.addView("REPL - History", new View("REPL - History", null, History));
        SplitWindow replSplit = new SplitWindow(false, ViewMap.getView("REPL - Execute"), ViewMap.getView("REPL - History"));
        
        ViewMap.getView("REPL - Execute").getWindowProperties().setCloseEnabled(false);
        ViewMap.getView("REPL - History").getWindowProperties().setCloseEnabled(false);
        
        // Create the error/debug/display views.
        Display = new NonEditableTextArea(this);
        Debug = new NonEditableTextArea(this);
        ViewMap.addView("Display", new View("Display", null, Display));
        ViewMap.addView("Debug", new View("Debug", null, Debug));
        ErrorManager.addErrorListener(new ErrorListener() {
			@Override
			public void logError(String msg) {
				Debug.append(msg + "\n");
			}
        });
        
        // Put everything together into the actual dockable display.
        SplitWindow fullSplit = new SplitWindow(false, 0.6f, documents, replSplit);
        Root.setWindow(fullSplit);
        add(Root);
        
        // Connect to Kawa.
        OutPort.setOutDefault(new SchemePrinter("Display", Display));
        OutPort.setErrDefault(new SchemePrinter("Display", Display));
        Kawa = new KawaWrap();
        
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

        History.append("\n� " + command.replace("\n", "\n  ") + "\n");
        
        Object result = Kawa.eval(command);
        if (result != null)
        	History.append(result.toString() + "\n");
    }

    /**
     * Update the display.
     */
	public boolean updateDisplay() {
		boolean reloaded = true;
		for (SchemeTextArea ss : new SchemeTextArea[]{History, Display, Debug, REPL}) {
			try {
				((SchemeDocument) ss.code.getDocument()).processChangedLines(0, ss.getText().length());
				ss.updateUI();
	    	}  catch (BadLocationException e) {
	    		reloaded = false;
	    		ErrorManager.logError("Unable to format view: " + e.getMessage());
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

	/**
	 * Show the debug view.
	 */
	public void showDebug() {
		View view = ViewMap.getView("Debug");
		
		if (!view.isShowing()) {
			if (view.getSize().width == 0 || view.getSize().height == 0)
				view.setSize(400, 400);
			
			FloatingWindow win = Root.createFloatingWindow(getLocation(), view.getSize(), view);
			win.getTopLevelAncestor().setVisible(true);
		}
	}

	/**
	 * Show the given view.
	 * @param which Which display we are writing to.
	 */
	public void showView(String which) {
		View view = ViewMap.getView(which);
		
		if (!view.isShowing()) {
			if (view.getSize().width == 0 || view.getSize().height == 0)
				view.setSize(200, 200);
			
			
			FloatingWindow win = Root.createFloatingWindow(getLocation(), view.getSize(), view);
			win.getTopLevelAncestor().setVisible(true);
		}
	}
}
