package actions;

import icons.IconManager;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

/**
 * Close the program.
 */
public class Exit extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;

	public Exit() {
		super("Exit", IconManager.icon("Exit.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames())
			frame.dispose();
	}
}