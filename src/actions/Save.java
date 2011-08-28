package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import gui.*;
import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Save the active document.
 */
public class Save extends AbstractAction {
	private static final long serialVersionUID = -1947196514315440029L;

	public Save() {
		super("Save", IconManager.icon("Save.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Save();
	}
}