package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.DocumentManager;

/**
 * Open a new document.
 */
public class Open extends AbstractAction {
	private static final long serialVersionUID = -792353524081571L;

	public Open() {
		super("Open", IconManager.icon("Open.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Open();
	}
}