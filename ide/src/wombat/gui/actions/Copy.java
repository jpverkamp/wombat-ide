package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

import wombat.gui.icons.IconManager;

/**
 * Copy selected text.
 */
public class Copy extends AbstractAction {
	private static final long serialVersionUID = -5442317793842147955L;
	
	Action todo;
	/**
	 * Copy constructor, puts the Action in the MenuManager's Map,
	 * links the Copy.png Image to the Object
	 * @return a Copy object
	 */
	public Copy() {
		super("Copy", IconManager.icon("Copy.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		todo = new DefaultEditorKit.CopyAction();
	}
	/**
	 * perform the Copy Action 
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see ActionEvent,DefaultEditorKit
	 */
	public void actionPerformed(ActionEvent arg0) {
		todo.actionPerformed(arg0);
	}
}