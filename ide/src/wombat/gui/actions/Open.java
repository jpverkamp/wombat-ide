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
 * Open an existing document.
 */
public class Open extends AbstractAction {
	private static final long serialVersionUID = -792353524081571L;

	/**
	 * Create the open action.
	 */
	public Open() {
		super("Open", IconManager.icon("Open.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Tell the DocumentManger to open a document.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Open();
	}
}