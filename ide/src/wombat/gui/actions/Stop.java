package wombat.gui.actions;


import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

import wombat.gui.frames.MainFrame;
import wombat.gui.icons.IconManager;

/**
 * Stop all running threads.
 */
public class Stop extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;

	/**
	 * Stop constructor, puts the Action in the MenuManager's Map,
	 * links the Stop.png Image to the Object
	 * @return a Stop object
	 */
	public Stop() {
		super("Stop", IconManager.icon("Stop.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Stop (Document) function. It stops all the 
	 * Threads that are running for the MainFrame
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see MainFrame, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames())
			if (frame.isVisible() && frame instanceof MainFrame)
				((MainFrame) frame).stopAllThreads(false, true);
	}
}