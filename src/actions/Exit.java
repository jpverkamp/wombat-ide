package actions;

import java.awt.event.ActionEvent;

import gui.*;

import javax.swing.AbstractAction;

/**
 * Close the program.
 */
public class Exit extends AbstractAction {
	private static final long serialVersionUID = -7508553925521875759L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().dispose();
	}
}