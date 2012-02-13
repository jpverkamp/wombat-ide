package wombat.gui.actions;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

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
