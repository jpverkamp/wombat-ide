package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.DocumentManager;

/**
 * Return/enter hit in the current active document.
 */
public class Return extends AbstractAction {
	private static final long serialVersionUID = 4079540525618774444L;

	public Return() {
		super("Return", IconManager.icon("Return.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Tab(true);
	}
}
