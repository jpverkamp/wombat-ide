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
 * Cut selected text by wrapping the default cut action.
 */
public class Cut extends AbstractAction {
	private static final long serialVersionUID = 718914853418341863L;

	Action OriginalCut;
	
	/**
	 * Create a cut action.
	 */
	public Cut() {
		super("Cut", IconManager.icon("Cut.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
		OriginalCut = new DefaultEditorKit.CutAction();
	}
	
	/**
	 * Perform the cut action.
	 * @param event Event parameters.
	 * @see DefaultEditorKit, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		OriginalCut.actionPerformed(event);
	}
}