package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.icons.IconManager;
import wombat.util.files.DocumentManager;

/**
 * Run the active document.
 */
public class Run extends AbstractAction {
	private static final long serialVersionUID = -7513211155903837433L;

	/**
	 * Run constructor, puts the Action in the MenuManager's Map,
	 * links the Run.png Image to the Object
	 * @return a Run object
	 */
	public Run() {
		super("Run", IconManager.icon("Run.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * performs the Run (Document) function
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DocumentManager, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		DocumentManager.Run();
	}
}