package actions;

import gui.MainFrame;
import icons.IconManager;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import wombat.launcher.Updater;

/**
 * Run the active document.
 */
public class Update extends AbstractAction {
	private static final long serialVersionUID = 418918561564535849L;

	public Update() {
		super("Update", IconManager.icon("Information.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	@Override
	public void actionPerformed(ActionEvent arg0) {
		if (JOptionPane.YES_OPTION == JOptionPane.showConfirmDialog(
				null, 
				"There is an update available, do you want to install it?\nThe update will run in the background.", 
				"Update Wombat?", 
				JOptionPane.YES_NO_OPTION)) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					try {
						Updater.updateAndReport();
					} catch (Exception e) {
						JOptionPane.showMessageDialog(null, "Unable to update Wombat:\n" + e.getMessage(), "Error", JOptionPane.OK_OPTION);
					}
				}
			});
			
			for (Frame frame : JFrame.getFrames())
				if (frame instanceof MainFrame)
					((MainFrame) frame).setUpdateVisible(false);
		}
	}
}