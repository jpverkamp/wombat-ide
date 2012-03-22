package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Undo the last action 
 */
public class Undo extends AbstractAction {
	private static final long serialVersionUID = 418918561564535849L;

	/**
	 * Undo constructor, puts the Action in the MenuManager's Map,
	 * links the Undo.png Image to the Object
	 * @return a Undo object
	 */
	public Undo() {
		super("Undo", IconManager.icon("Undo.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Undo (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Undo();
	}
}