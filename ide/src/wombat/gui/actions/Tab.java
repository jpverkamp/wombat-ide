package wombat.gui.actions;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Tab hit in the current active document.
 */
public class Tab extends AbstractAction {
	private static final long serialVersionUID = -5538888549174841916L;
	
	/**
	 * Tab constructor, puts the Action in the MenuManager's Map,
	 * links the Tab.png Image to the Object
	 * @return a Tab object
	 */
	public Tab() {
		super("Tab", IconManager.icon("Tab.png"));
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
		DocumentManager.Tab(false);
	}
}
