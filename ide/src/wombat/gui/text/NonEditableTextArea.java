/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text;

import java.awt.Dimension;

/**
 * Custom text area where editing isn't allowed. Used at the moment for the history pane. Can't rewrite history.
 */
public class NonEditableTextArea extends SchemeTextArea {
	private static final long serialVersionUID = -1184733261901876758L;

	/**
	 * Create the history text area. 
	 */
	public NonEditableTextArea() {
		super(false);
		
		setPreferredSize(new Dimension(100, getHeight() / 2 - 100));
        code.setEditable(false);
	}
	
    /**
     * Append text to the end of the code area.
     *
     * @param text Text to append.
     */
    public synchronized void append(String text) {
    	super.append(text);
    	goToEnd();
    }
}
