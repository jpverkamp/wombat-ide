package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Format the active document.
 */
public class Reset extends AbstractAction {
	private static final long serialVersionUID = -1234756890127385121L;

	public Reset() {
		super("Reset", IconManager.icon("Reset.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().kawa.reset();
		MainFrame.me().History.append("\n>>> Environment reset <<<\n");
	}
}