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
 * Display the find/replace dialog.
 */
public class FindReplace extends AbstractAction {
	private static final long serialVersionUID = -1486418641534138445L;

	/**
	 * Create the find/replace action.
	 */
	public FindReplace() {
		super("Find/Replace", IconManager.icon("FindReplace.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Display the find/replace dialog.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */ 
	public void actionPerformed(ActionEvent event) {
		DocumentManager.FindReplace();
	}
}