package wombat.gui.actions;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JFrame;

import wombat.gui.frames.MainFrame;

/**
 * Show the Debug into for MainFrame
 */
public class ShowError extends AbstractAction {
	private static final long serialVersionUID = 4777775801276799438L;

	/**
	 * Shows the debug info for the MainFrame
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see ActionEvent, MainFrame
	 */
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames())
			if (frame instanceof MainFrame)
				((MainFrame) frame).showDebug();
	}
}