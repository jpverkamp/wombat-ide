package actions;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;

/**
 * Run the active document.
 */
public class Run extends AbstractAction {
	private static final long serialVersionUID = -7513211155903837433L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.Run();
	}
}