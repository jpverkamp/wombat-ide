/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.text.DefaultEditorKit;

import wombat.gui.icons.IconManager;

/**
 * Copy selected text by wrapping the default copy action. 
 */
public class Copy extends AbstractAction {
	private static final long serialVersionUID = -5442317793842147955L;
	
	Action OriginalCopy;
	
	/**
	 * Create a copy action.
	 */
	public Copy() {
		super("Copy", IconManager.icon("Copy.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		OriginalCopy = new DefaultEditorKit.CopyAction();
	}
	
	/**
	 * Perform the copy. 
	 * @param event Event parameters.
	 * @see ActionEvent,DefaultEditorKit
	 */
	public void actionPerformed(ActionEvent event) {
		OriginalCopy.actionPerformed(event);
	}
}