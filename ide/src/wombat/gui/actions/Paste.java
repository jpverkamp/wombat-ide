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
 * Paste selected text from clip board by wrapping the default paste action.
 */
public class Paste extends AbstractAction {
	private static final long serialVersionUID = 8274755168128480452L;
	
	Action DefaultPaste;
	
	/**
	 * Create the new paste action.
	 */
	public Paste() {
		super("Paste", IconManager.icon("Paste.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		DefaultPaste = new DefaultEditorKit.PasteAction();
	}
	
	/**
	 * Actually do the paste.
	 * @param event Event parameters.
	 * @see DefaultEditorKit, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		DefaultPaste.actionPerformed(event);
	}
}