package actions;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;

/**
 * Format the active document.
 */
public class Format extends AbstractAction {
	private static final long serialVersionUID = -7992738631514081571L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.Format();
	}
}