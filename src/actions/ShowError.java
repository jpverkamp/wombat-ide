package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import gui.*;

import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Close the active document.
 */
public class ShowError extends AbstractAction {
	private static final long serialVersionUID = 4777775801276799438L;

	public ShowError() {
		super("ShowError", IconManager.icon("ShowError.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		ErrorFrame.showMe();
	}
}