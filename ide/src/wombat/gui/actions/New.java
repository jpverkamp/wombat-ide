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
 * Create a new document. 
 */
public class New extends AbstractAction {
	private static final long serialVersionUID = -8921545233355515110L;

	/**
	 * Create the new action.
	 */
	public New() {
		super("New", IconManager.icon("New.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Tell the document manager to create a new document.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.New();
	}
}
