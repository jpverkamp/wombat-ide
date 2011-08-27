package actions;

import gui.*;


import icons.IconManager;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.BadLocationException;

/**
 * Return/enter hit in the current active document.
 */
public class Return extends AbstractAction {
	private static final long serialVersionUID = 4079540525618774444L;

	public Return() {
		super("Return", IconManager.icon("Return.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		SchemeTextArea doc = MainFrame.me().Documents.activeDocument;
		
		if (doc == null)
			return;
		
		try {
			doc.code.getDocument().insertString(doc.code.getCaretPosition(), "\n", null);
        } catch (BadLocationException ble) {
            ErrorFrame.log("Unable to add a new line on ENTER.");
        }
		
		doc.tab();
	}
}
