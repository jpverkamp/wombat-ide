/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.frames.MainFrame;
import wombat.gui.icons.IconManager;

/**
 * Rest the embedded scheme interpreter (clean the environment).
 */
public class Reset extends AbstractAction {
	private static final long serialVersionUID = -1234756890127385121L;

	/**
	 * Create the reset action.
	 */
	public Reset() {
		super("Reset", IconManager.icon("Reset.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Reset the scheme environment.
	 * @param event Event parameters (ignored).
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		MainFrame.Singleton().resetScheme();
	}
}