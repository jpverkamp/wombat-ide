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
 * Show debug information.
 */
public class ShowError extends AbstractAction {
	private static final long serialVersionUID = 4777775801276799438L;

	/**
	 * Create an action to show the about dialog.
	 */
	public ShowError() {
		super("ShowError", IconManager.icon("ShowError.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Shows the debug information .
	 * @param event Event parameters (ignored).
	 * @see ActionEvent, MainFrame
	 */
	public void actionPerformed(ActionEvent event) {
		MainFrame.Singleton().showDebug();
	}
}