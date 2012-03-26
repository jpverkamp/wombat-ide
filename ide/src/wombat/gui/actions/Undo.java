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
 * Roll back one step on the undo stack. 
 */
public class Undo extends AbstractAction {
	private static final long serialVersionUID = 418918561564535849L;

	/**
	 * Create an Undo action.
	 */
	public Undo() {
		super("Undo", IconManager.icon("Undo.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Performs an undo in the active document.
	 * @param event EVent parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Undo();
	}
}