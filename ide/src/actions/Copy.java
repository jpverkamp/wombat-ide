package actions;

import icons.IconManager;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

/**
 * Cut selected text.
 */
public class Copy extends AbstractAction {
	private static final long serialVersionUID = -5442317793842147955L;
	
	Action todo;
	
	public Copy() {
		super("Copy", IconManager.icon("Copy.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		
		todo = new DefaultEditorKit.CopyAction();
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		todo.actionPerformed(arg0);
	}
}