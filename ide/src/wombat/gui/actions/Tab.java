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
 * Tab hit in the current active document, move the current line to the correct indentation.
 */
public class Tab extends AbstractAction {
	private static final long serialVersionUID = -5538888549174841916L;
	
	/**
	 * Create the tab action.
	 */
	public Tab() {
		super("Tab", IconManager.icon("Tab.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	/**
	 * Correctly indent the current line in the active document.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Tab(false);
	}
}
