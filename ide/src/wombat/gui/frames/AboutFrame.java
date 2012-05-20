/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.frames;

import java.awt.Desktop;
import java.io.IOException;
import java.util.Calendar;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.event.HyperlinkEvent.EventType;
import javax.swing.text.html.*;

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
        setSize(400, 400);
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setLocationByPlatform(true);
        
        String text = "Failed to load about page";
        try {
        	text = FileAccess.getFile("wombat/gui/frames/about.htm", true)
					.replace("{year}", "" + Calendar.getInstance().get(Calendar.YEAR))
					.replace("{version}", Wombat.VERSION);
        } catch(IOException ex) {
        }
        
        JEditorPane license = new JEditorPane();
        HTMLEditorKit kit = new HTMLEditorKit();
        StyleSheet style = kit.getStyleSheet();
        style.addRule("a { color: blue; }");
        license.setEditorKit(kit);
        HTMLDocument doc = (HTMLDocument) license.getDocument();
		
        try {
        	kit.insertHTML(doc, 0, text, 0, 0, null);
		} catch (Exception e) {
			e.printStackTrace();
		}
        license.setEditable(false);
		add(new JScrollPane(license));
		
		license.addHyperlinkListener(new HyperlinkListener() {
			@Override public void hyperlinkUpdate(HyperlinkEvent event) {
				if (event.getEventType() == EventType.ACTIVATED) { 
					try {
						Desktop.getDesktop().browse(event.getURL().toURI());
					} catch (Exception e) {
					}
				}
			}
		});
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