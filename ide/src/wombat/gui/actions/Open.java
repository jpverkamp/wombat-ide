package wombat.gui.actions;


import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Open a new document.
 */
public class Open extends AbstractAction {
	private static final long serialVersionUID = -792353524081571L;

	/**
	 * Open constructor, puts the Action in the MenuManager's Map,
	 * links the Open.png Image to the Object
	 * @return a Open object
	 */
	public Open() {
		super("Open", IconManager.icon("Open.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 *  performs the Open (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Open();
	}
}