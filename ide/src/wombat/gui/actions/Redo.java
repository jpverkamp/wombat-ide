package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Run the active document.
 */
public class Redo extends AbstractAction {
	private static final long serialVersionUID = 448918915341534569L;

	/**
	 * Redo constructor, puts the Action in the MenuManager's Map,
	 * links the Redo.png Image to the Object
	 * @return a Redo object
	 */
	public Redo() {
		super("Redo", IconManager.icon("Redo.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Redo (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Redo();
	}
}