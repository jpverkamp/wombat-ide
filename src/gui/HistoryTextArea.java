package gui;

import java.awt.Dimension;

/**
 * Store command history.
 */
public class HistoryTextArea extends SchemeTextArea {
	private static final long serialVersionUID = -1184733261901876758L;

	/**
	 * Create the history text area. 
	 */
	public HistoryTextArea() {
		setPreferredSize(new Dimension(100, getHeight() / 2 - 100));
        code.setEditable(false);
	}
}
