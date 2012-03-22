package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Save the active document.
 */
public class Save extends AbstractAction {
	private static final long serialVersionUID = -1947196514315440029L;

	/**
	 * Save constructor, puts the Action in the MenuManager's Map,
	 * links the Save.png Image to the Object
	 * @return a Save object
	 */
	public Save() {
		super("Save", IconManager.icon("Save.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Save (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Save();
	}
}