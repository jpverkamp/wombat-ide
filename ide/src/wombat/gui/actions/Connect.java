/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.actions;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.InetAddress;

import javax.swing.*;

import wombat.gui.frames.*;
import wombat.gui.icons.*;
import wombat.util.files.DocumentManager;

/**
 * Connect and share a document. Can be used either for hosting or joining a previously hosted document.
 */
public class Connect extends AbstractAction {
	private static final long serialVersionUID = 3786293931035177076L;

	
	/**
	 * Create a connect object.
	 */
	public Connect() {
		super("Connect", IconManager.icon("Connect.png"));
		putValue(Action.SHORT_DESCRIPTION, getValue(Action.NAME));
	}
	
	/**
	 * Display the ConnectDialog to arrange sharing. 
	 * @param event Action parameters (ignored)
	 * @see ConnectDialog, ActionEvent
	 */
	public void actionPerformed(ActionEvent event) {
		// Are they going to host or join?
		final JDialog hostOptions = new JDialog(MainFrame.Singleton(), "Shared Document");
		hostOptions.setLayout(new GridLayout(3, 1));
		hostOptions.setModal(true);
		hostOptions.setLocationByPlatform(true);
		
		final JButton host = new JButton("Create a new document");
		final JButton join = new JButton("Join an existing document");
		final JButton cancel = new JButton("Cancel");
		
		ActionListener al = new ActionListener() {
			@Override public void actionPerformed(ActionEvent e) {
				hostOptions.setVisible(false);
				
				if (e.getSource() == host) {
					
					try {
						DocumentManager.HostShared();
					} catch (Exception ex) {
						ex.printStackTrace();
						JOptionPane.showMessageDialog(MainFrame.Singleton(), "Cannot host server");
					}
					
				} else if (e.getSource() == join) {
					
					// Display a dialog asking for a name.
					String name = (String) JOptionPane.showInputDialog(
							MainFrame.Singleton(), 
							"Enter the server to connect to in the form IP:PORT", 
							"Server",
							JOptionPane.QUESTION_MESSAGE,
							null,
							null,
							"192.168.1.50:5309");
					
					// If they didn't choose a name, just bail out.
					if (name == null) 
						return;
					
					// Try to connect.
					try {
						String[] parts = name.split(":");
						DocumentManager.JoinShared(InetAddress.getByName(parts[0]), Integer.parseInt(parts[1]));
					} catch(Exception ex) {
						ex.printStackTrace();
						JOptionPane.showMessageDialog(MainFrame.Singleton(), "Cannot connect to server");
					}
					
				} else if (e.getSource() == cancel) {
					
				}
			}
		};
		
		host.addActionListener(al);
		join.addActionListener(al);
		cancel.addActionListener(al);
		
		hostOptions.add(host);
		hostOptions.add(join);
		hostOptions.add(cancel);
		
		hostOptions.pack();
		hostOptions.setVisible(true);
	}
}
