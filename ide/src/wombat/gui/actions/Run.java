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
 * Load and run the active document in the embedded scheme interpreter.
 */
public class Run extends AbstractAction {
	private static final long serialVersionUID = -7513211155903837433L;

	/**
	 * Create the run action.
	 */
	public Run() {
		super("Run", IconManager.icon("Run.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Run the active document in scheme.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Run();
	}
}