package actions;

import java.awt.event.ActionEvent;

import gui.*;

import javax.swing.AbstractAction;

/**
 * Close the active document.
 */
public class ShowError extends AbstractAction {
	private static final long serialVersionUID = 4777775801276799438L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		ErrorFrame.showMe();
	}
}