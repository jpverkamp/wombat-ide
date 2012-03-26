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
 * Save the active document with a new name.
 */
public class SaveAs extends AbstractAction {
	private static final long serialVersionUID = 7105447480725716711L;

	/**
	 * Create the save as action.
	 */
	public SaveAs() {
		super("SaveAs", IconManager.icon("SaveAs.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Save the active document with a new name.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.SaveAs();
	}
}