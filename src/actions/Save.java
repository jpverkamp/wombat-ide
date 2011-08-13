package actions;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;

/**
 * Save the active document.
 */
public class Save extends AbstractAction {
	private static final long serialVersionUID = -1947196514315440029L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.Save();
	}
}