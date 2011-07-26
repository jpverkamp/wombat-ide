package gui;

import java.util.*;
import javax.swing.*;

/**
 * About frame.
 */
public class AboutFrame extends JFrame {
    static AboutFrame me;
    
    private AboutFrame () {
        setTitle("About Wombat");
        setSize(600, 400);
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        
        Scanner s = new Scanner(getClass().getResourceAsStream("/License.txt"));
        StringBuilder sb = new StringBuilder();
        while (s.hasNextLine())
            sb.append(s.nextLine() + "\n");
        s.close();
        
        JTextArea license = new JTextArea(sb.toString());
        JScrollPane scroll = new JScrollPane(license);
        
        license.setEditable(false);
        
        add(scroll);
    }
    
    public static AboutFrame me() {
        if (me == null)
            me = new AboutFrame();
        
        return me;
    }
}
