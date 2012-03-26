/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.*;

import wombat.gui.frames.*;
import wombat.gui.icons.*;

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
		// Find the main frame to connect to.
		new ConnectDialog(MainFrame.Singleton()).setVisible(true);
	}
}