package actions;
import icons.IconManager;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.DocumentManager;

/**
 * Tab hit in the current active document.
 */
public class Tab extends AbstractAction {
	private static final long serialVersionUID = -5538888549174841916L;
	
	public Tab() {
		super("Tab", IconManager.icon("Tab.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Tab(false);
	}
}
