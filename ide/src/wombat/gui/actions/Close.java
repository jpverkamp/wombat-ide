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
	 * Close constructor, puts the Action in the MenuManager's Map,
	 * links the Close.png Image to the Object
	 * @return a Close object
	 */
	public Close() {
		super("Close", IconManager.icon("Close.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs DocumentManager's Close function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Close(false);
	}
}