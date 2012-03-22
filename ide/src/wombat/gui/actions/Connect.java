package wombat.gui.actions;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.*;

import wombat.gui.frames.*;
import wombat.gui.icons.*;

/**
 * Connect and share a document.
 */
public class Connect extends AbstractAction {
	private static final long serialVersionUID = 3786293931035177076L;
	/**
	 * Connect constructor, puts the Action in the MenuManager's Map,
	 * links the Connect.png Image to the Object
	 * @return a Connect object
	 */
	public Connect() {
		super("Connect", IconManager.icon("Connect.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * share the frames via ConnectDialog 
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see ConnectDialog, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames()) {
			if (frame instanceof MainFrame) {
				new ConnectDialog((MainFrame) frame).setVisible(true);
				return;
			}
		}
	}
}