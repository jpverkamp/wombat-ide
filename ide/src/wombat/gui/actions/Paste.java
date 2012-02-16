package wombat.gui.actions;


import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

import wombat.gui.icons.IconManager;

/**
 * Cut selected text.
 */
public class Paste extends AbstractAction {
	private static final long serialVersionUID = 8274755168128480452L;
	
	Action todo;
	
	public Paste() {
		super("Paste", IconManager.icon("Paste.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		
		todo = new DefaultEditorKit.PasteAction();
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		todo.actionPerformed(arg0);
	}
}