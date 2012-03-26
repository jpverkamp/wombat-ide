/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

import wombat.gui.icons.IconManager;

/**
 * Close the program.
 */
public class Exit extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;
	
	/**
	 * Create the exit action.
	 */
	public Exit() {
		super("Exit", IconManager.icon("Exit.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Close the program.
	 * @param event Event parameters (ignored).
	 * @see DefaultEditorKit, ActionEvent, JFrame, Frame
	 */ 
	public void actionPerformed(ActionEvent arg0) {
		// Go through all frames and dispose them.
		// This will trigger the the MainFrame's dispose at some point which does most of the work.
		for (Frame frame : JFrame.getFrames())
			frame.dispose();
	}
}