/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.frames.AboutFrame;
import wombat.gui.icons.IconManager;

/**
 * Show the About Frame.
 */
public class ShowAbout extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;

	/**
	 * Create an action to show the about dialog.
	 */
	public ShowAbout() {
		super("ShowAbout", IconManager.icon("ShowAbout.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Shows the about dialog.
	 * @param event Event parameters (ignored).
	 * @see AboutFrame, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		AboutFrame.showMe();
	}
}