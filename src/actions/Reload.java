package actions;

import java.awt.event.ActionEvent;

import gui.*;

import javax.swing.AbstractAction;

/**
 * Close the active document.
 */
public class Reload extends AbstractAction {
	private static final long serialVersionUID = 6173253035856442086L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		Options.reload();
	}
}