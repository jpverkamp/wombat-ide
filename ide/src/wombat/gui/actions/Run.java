package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Run the active document.
 */
public class Run extends AbstractAction {
	private static final long serialVersionUID = -7513211155903837433L;

	public Run() {
		super("Run", IconManager.icon("Run.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Run();
	}
}