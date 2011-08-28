package actions;

import gui.*;

import icons.IconManager;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

/**
 * Create a new document. 
 */
public class New extends AbstractAction {
	private static final long serialVersionUID = -8921545233355515110L;

	public New() {
		super("New", IconManager.icon("New.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.New();
	}
}
