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
 * Stop the running scheme process.
 */
public class Stop extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;

	/**
	 * Create the stop action.
	 */
	public Stop() {
		super("Stop", IconManager.icon("Stop.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Stop the scheme interpreter (actually kills and restarts the scheme process).
	 * @param event Event parameters (ignored).
	 * @see MainFrame, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		MainFrame.Singleton().stopAllThreads(false,  true);
	}
}