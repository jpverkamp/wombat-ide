/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.frames;

import java.io.FileNotFoundException;
import java.util.Calendar;
import javax.swing.*;

import wombat.Wombat;
import wombat.util.files.FileAccess;

/**
 * Display information about this program.
 */
public class AboutFrame extends JFrame {
	private static final long serialVersionUID = -4920859302858551323L;

	// Make it a singleton.
	static AboutFrame me;

	/**
	 * Create the about frame.
	 * TODO: Load from file embeded in the source code.
	 */
    private AboutFrame () {
        setTitle("About Wombat");
        setSize(800, 700);
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        
        JLabel license;
		try {
			license = new JLabel(
				FileAccess.getFile("wombat/gui/frames/about.htm", true)
					.replace("{year}", "" + Calendar.getInstance().get(Calendar.YEAR))
					.replace("{version}", Wombat.VERSION),
				JLabel.CENTER);
		} catch (FileNotFoundException e) {
			license = new JLabel("Unable to load about.htm");
		}
		add(license);
    }
    
    /**
	 * Shows the About Frame
	 * @return void
	 * @see JFrame
	 */
    	public static void showMe() {
            if (me == null)
                me = new AboutFrame();

            me.setVisible(true);
        }
}