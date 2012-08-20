/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.*;

import wombat.gui.frames.*;
import wombat.gui.icons.*;
import wombat.util.NameGenerator;
import wombat.util.files.DocumentManager;

/**
 * Connect and share a document. Can be used either for hosting or joining a previously hosted document.
 */
public class Connect extends AbstractAction {
	private static final long serialVersionUID = 3786293931035177076L;

	
	/**
	 * Create a connect object.
	 */
	public Connect() {
		super("Connect", IconManager.icon("Connect.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Display the ConnectDialog to arrange sharing. 
	 * @param event Action parameters (ignored)
	 * @see ConnectDialog, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		// Display a dialog asking for a name.
		String name = (String) JOptionPane.showInputDialog(
				MainFrame.Singleton(), 
				"Choose a document name:", 
				"Document name",
				JOptionPane.QUESTION_MESSAGE, 
				null,
				null, 
				NameGenerator.getName());
		
		// If they didn't choose a name, just bail out.
		if (name == null) 
			return;
		
		// Try to connect.
		try {
			DocumentManager.NewShared(name);
		} catch(Exception ex) {
			ex.printStackTrace();
//			JOptionPane.showMessageDialog(this, "Cannot host server:\n" + ex.getMessage(), "Cannot host", JOptionPane.OK_OPTION);
		}
	}
}