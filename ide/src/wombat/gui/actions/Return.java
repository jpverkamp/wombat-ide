package wombat.gui.actions;


import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Return/enter hit in the current active document.
 */
public class Return extends AbstractAction {
	private static final long serialVersionUID = 4079540525618774444L;

	/**
	 * Return constructor, puts the Action in the MenuManager's Map,
	 * links the Return.png Image to the Object
	 * @return a Return object
	 */
	public Return() {
		super("Return", IconManager.icon("Return.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	/**
	 * performs the Tab (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Tab(true);
	}
}
