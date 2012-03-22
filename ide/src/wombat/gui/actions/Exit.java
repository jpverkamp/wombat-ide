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
	 * Exit constructor, puts the Action in the MenuManager's Map,
	 * links the Exit.png Image to the Object
	 * @return a Exit object
	 */
	public Exit() {
		super("Exit", IconManager.icon("Exit.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	/**
	 * gets rid of all the projects frames.
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DefaultEditorKit, ActionEvent, JFrame, Frame
	 */ 
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames())
			frame.dispose();
	}
}