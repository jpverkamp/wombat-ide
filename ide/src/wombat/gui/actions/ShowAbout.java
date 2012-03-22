package wombat.gui.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.frames.AboutFrame;
import wombat.gui.icons.IconManager;

/**
 * Show the About Frame.
 */
public class ShowAbout extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;

	/**
	 * ShowAbout constructor, puts the Action in the MenuManager's Map,
	 * links the ShowAbout.png Image to the Object
	 * @return a ShowAbouts object
	 */
	public ShowAbout() {
		super("ShowAbout", IconManager.icon("ShowAbout.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Shows the About Frame
	 * @param arg0  an ActionEvent that triggers the method call
	 * @return void
	 *  
	 * @see AboutFrame, ActionEvent
	 */
	public void actionPerformed(ActionEvent arg0) {
		AboutFrame.showMe();
	}
}