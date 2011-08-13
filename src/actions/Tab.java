package actions;

import gui.*;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;;

/**
 * Tab hit in the current active document.
 */
public class Tab extends AbstractAction {
	private static final long serialVersionUID = -5538888549174841916L;
	
	public Tab() {
		super("Tab");
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		SchemeTextArea doc = MainFrame.me().Documents.activeDocument;
		
		if (doc == null)
			return;
		
		doc.format();
	}
}
