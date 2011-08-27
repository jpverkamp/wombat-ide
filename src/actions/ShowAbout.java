package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import gui.*;

import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Close the active document.
 */
public class ShowAbout extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;

	public ShowAbout() {
		super("ShowAbout", IconManager.icon("ShowAbout.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		AboutFrame.showMe();
	}
}