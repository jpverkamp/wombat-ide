package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Format the active document.
 */
public class FindReplace extends AbstractAction {
	private static final long serialVersionUID = -1486418641534138445L;

	public FindReplace() {
		super("Find/Replace", IconManager.icon("FindReplace.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.FindReplace();
	}
}