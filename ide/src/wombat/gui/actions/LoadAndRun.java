/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JOptionPane;

import wombat.gui.frames.MainFrame;
import wombat.gui.icons.IconManager;

/**
 * Load a file from disk without putting it in the REPL.
 */
public class LoadAndRun extends AbstractAction {
	private static final long serialVersionUID = -8025511385554029396L;

	/**
	 * Create the open action.
	 */
	public LoadAndRun() {
		super("Load into Scheme", IconManager.icon("LoadAndRun.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Tell the DocumentManger to open a document.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		System.out.println("load and run");
		
		if (MainFrame.Singleton().ToolBarStop.isEnabled()) {
			JOptionPane.showMessageDialog(MainFrame.Singleton(), "Unable to load file, Scheme process busy.", "Error in load", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		FileDialog fc = new FileDialog(MainFrame.Singleton(), "Load into Scheme", FileDialog.LOAD);
        fc.setVisible(true);

        if (fc.getFile() == null)
        	return;

        File file = new File(fc.getDirectory(), fc.getFile());
        try {
	        if (!file.exists()) {
	        	JOptionPane.showMessageDialog(MainFrame.Singleton(), "Unable to load file '" + file.getCanonicalPath() + "', file does not exist.", "Error in load", JOptionPane.ERROR_MESSAGE);
				return;
	        }
	        
	        MainFrame.Singleton().doCommand("(load \"" + file.getCanonicalPath().replace("\\", "\\\\") + "\")");
        } catch (IOException ex) {
        	JOptionPane.showMessageDialog(MainFrame.Singleton(), "Unable to load file: " + ex.getMessage(), "Error in load", JOptionPane.ERROR_MESSAGE);
			return;
        }
	}
}