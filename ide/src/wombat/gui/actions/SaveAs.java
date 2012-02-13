package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Save the active document with a new name.
 */
public class SaveAs extends AbstractAction {
	private static final long serialVersionUID = 7105447480725716711L;

	public SaveAs() {
		super("SaveAs", IconManager.icon("SaveAs.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.SaveAs();
	}
}