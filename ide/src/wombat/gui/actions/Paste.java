package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

import wombat.gui.icons.IconManager;

/**
 * Paste selected text from clip board.
 */
public class Paste extends AbstractAction {
	private static final long serialVersionUID = 8274755168128480452L;
	
	Action todo;
	
	/**
	 * Paste constructor, puts the Action in the MenuManager's Map,
	 * links the Paste.png Image to the Object
	 * @return a Paste object
	 */
	public Paste() {
		super("Paste", IconManager.icon("Paste.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));//this adds the Action to the Button Hashmap
		
		// make the Action a Paste Action
		todo = new DefaultEditorKit.PasteAction();
	}
	/**
	 * performs the Paste action
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DefaultEditorKit, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		todo.actionPerformed(arg0);
	}
}