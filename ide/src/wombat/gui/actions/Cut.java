package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

import wombat.gui.icons.IconManager;

/**
 * Cut selected text.
 */
public class Cut extends AbstractAction {
	private static final long serialVersionUID = 718914853418341863L;

	Action todo;
	/**
	 * Cut constructor, puts the Action in the MenuManager's Map,
	 * links the Cut.png Image to the Object
	 * @return a Cut object
	 */
	public Cut() {
		super("Cut", IconManager.icon("Cut.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		todo = new DefaultEditorKit.CutAction();
	}
	/**
	 * perform the Cut action
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see DefaultEditorKit, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		todo.actionPerformed(arg0);
	}
}