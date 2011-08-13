package actions;

import gui.*;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

/**
 * Create a new document. 
 */
public class New extends AbstractAction {
	private static final long serialVersionUID = -8921545233355515110L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		MainFrame.me().Documents.New();
	}
}
