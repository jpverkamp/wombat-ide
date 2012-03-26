/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Close the active document.
 */
public class Close extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;
	
	/**
	 * Setup the close action.
	 */
	public Close() {
		super("Close", IconManager.icon("Close.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Close the active document.
	 * @param event Action parameters.
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DocumentManager.Close(false);
	}
}