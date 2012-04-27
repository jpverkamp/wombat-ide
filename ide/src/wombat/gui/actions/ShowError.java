/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileInputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;

import wombat.gui.frames.MainFrame;
import wombat.gui.icons.IconManager;

/**
 * Show debug information.
 */
public class ShowError extends AbstractAction {
	private static final long serialVersionUID = 4777775801276799438L;

	/**
	 * Create an action to show the about dialog.
	 */
	public ShowError() {
		super("ShowError", IconManager.icon("ShowError.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Shows the debug information .
	 * @param event Event parameters (ignored).
	 * @see ActionEvent, MainFrame
	 */
	public void actionPerformed(ActionEvent event) {
		StringBuffer sb = new StringBuffer();
		sb.append("Logs:\n\n");
		
		for (String s : new File(".").list()) {
			File f = new File(s);
			if (s.startsWith("log") && f.isFile() && f.canRead()) {
				try {
					sb.append("-- " + f.getCanonicalPath() + " --");
					
					byte[] buffer = new byte[(int) f.length()];
				    FileInputStream fis = new FileInputStream(f);
				    fis.read(buffer);
					sb.append(new String(buffer));
				} catch(Exception e) {
					
				}
			}
		}
		
		MainFrame.Singleton().DebugLogs.setText(sb.toString());
		
		MainFrame.Singleton().showDebug();
	}
}