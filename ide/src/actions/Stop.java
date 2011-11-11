package actions;

import gui.MainFrame;
import icons.IconManager;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

/**
 * Stop all running threads.
 */
public class Stop extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;

	public Stop() {
		super("Stop", IconManager.icon("Stop.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		for (Frame frame : JFrame.getFrames())
			if (frame.isVisible() && frame instanceof MainFrame)
				((MainFrame) frame).stopAllThreads();
	}
}