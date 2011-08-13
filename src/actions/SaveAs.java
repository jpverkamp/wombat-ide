package actions;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;

/**
 * Save the active document with a new name.
 */
public class SaveAs extends AbstractAction {
	private static final long serialVersionUID = 7105447480725716711L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.SaveAs();
	}
}