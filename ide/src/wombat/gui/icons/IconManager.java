/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.icons;

import java.net.URL;
import java.util.*;
import javax.swing.*;

/**
 * Manage icons to load them from JARs and don't load them more than once.
 */
public class IconManager {
	static Map<String, ImageIcon> icons = new HashMap<String, ImageIcon>();
	
	private IconManager() {}
	
	/**
	 * Fetch an icon by name, caching queries..
	 * 
	 * @param name The icon's name.
	 * @return The icon.
	 */
	public static ImageIcon icon(String name) {
		if (!icons.containsKey(name)) {
			URL iconURL = null;
			
			// Try all the ways that the icon could be stored, depending on if it's in a JAR or directly on the OS.
			// If none of these work, fall back to the default of no icon.
			if (iconURL == null) iconURL = IconManager.class.getResource("/icons/" + name);
			if (iconURL == null) iconURL = IconManager.class.getResource("icons/" + name);
			if (iconURL == null) iconURL = IconManager.class.getResource("/wombat/gui/icons/" + name);
			if (iconURL == null) iconURL = IconManager.class.getResource("wombat/gui/icons/" + name);
			if (iconURL == null) iconURL = IconManager.class.getResource(name);
			
			if (iconURL == null) 
				icons.put(name, null);
			else 
				icons.put(name, new ImageIcon(iconURL));
		}
			
		return icons.get(name); 
	}

}
