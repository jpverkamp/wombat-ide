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

	public Connect() {
		super("Connect", IconManager.icon("Connect.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames()) {
			if (frame instanceof MainFrame) {
				new ConnectDialog((MainFrame) frame).setVisible(true);
				return;
			}
		}
	}
}