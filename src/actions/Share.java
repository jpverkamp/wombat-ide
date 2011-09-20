package actions;

import gui.ShareDialog;
import icons.IconManager;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Share the active document.
 */
public class Share extends AbstractAction {
	private static final long serialVersionUID = 7105447480725716711L;

	public Share() {
		super("Share", IconManager.icon("Share.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		ShareDialog.show();
	}
}