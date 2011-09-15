package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.DocumentManager;

/**
 * Run the active document.
 */
public class Undo extends AbstractAction {
	private static final long serialVersionUID = 418918561564535849L;

	public Undo() {
		super("Undo", IconManager.icon("Undo.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Undo();
	}
}