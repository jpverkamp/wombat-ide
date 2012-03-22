package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Format the active document.
 */
public class Format extends AbstractAction {
	private static final long serialVersionUID = -7992738631514081571L;

	/**
	 * Format constructor, puts the Action in the MenuManager's Map,
	 * links the Format.png Image to the Object
	 * @return a Format object
	 */
	public Format() {
		super("Format", IconManager.icon("Format.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Format (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Format();
	}
}