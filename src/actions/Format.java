package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Format the active document.
 */
public class Format extends AbstractAction {
	private static final long serialVersionUID = -7992738631514081571L;

	public Format() {
		super("Format", IconManager.icon("Format.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.Format();
	}
}