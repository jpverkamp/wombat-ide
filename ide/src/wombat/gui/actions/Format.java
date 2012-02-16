package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

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
		DocumentManager.Format();
	}
}