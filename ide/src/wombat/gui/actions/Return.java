/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Trigger the return/enter key in the current active document.
 */
public class Return extends AbstractAction {
	private static final long serialVersionUID = 4079540525618774444L;

	/**
	 * Create the return action.
	 */
	public Return() {
		super("Return", IconManager.icon("Return.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	/**
	 * Hit return/enter. Essentially, insert a newline then tab to the correct depth.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Tab(true);
	}
}
