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
 * Format the active document (automatically reindent all fo the lines).
 */
public class Format extends AbstractAction {
	private static final long serialVersionUID = -7992738631514081571L;

	/**
	 * Create the format action.
	 */
	public Format() {
		super("Format", IconManager.icon("Format.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Format the active document.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Format();
	}
}