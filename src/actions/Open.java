package actions;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;

/**
 * Open a new document.
 */
public class Open extends AbstractAction {
	private static final long serialVersionUID = -792353524081571L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.Open();
	}
}