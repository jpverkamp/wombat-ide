package actions;

import gui.*;

import icons.IconManager;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;

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
		SchemeTextArea doc = MainFrame.me().Documents.activeDocument;
		
		if (doc == null)
			return;
		
		doc.tab();
	}
}
