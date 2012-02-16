package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Close the active document.
 */
public class Close extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;

	public Close() {
		super("Close", IconManager.icon("Close.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Close(false);
	}
}