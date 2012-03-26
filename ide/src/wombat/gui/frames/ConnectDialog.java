/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.frames;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import wombat.util.files.DocumentManager;

/**
 * Dialog to connect shared documents between different machines. 
 */
public class ConnectDialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = 4997826705741440876L;

	// Singleton.
	static ConnectDialog me;
	
	// Contain the address to connect to.
	JTextField joinAddress;
	
	/**
	 * Create a new connection dialog. 
	 * @param parent The main frame hosting this dialog.
	 */
	public ConnectDialog(MainFrame parent) {
		super(parent, true);
		
		setBackground(Color.RED);
		setLocationByPlatform(true);
		setTitle("Connect...");
		setResizable(false);
		setLayout(new GridBagLayout());
		
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridheight = 1;
		gbc.insets = new Insets(5, 5, 5, 5);
		gbc.fill = GridBagConstraints.BOTH;
		
		JButton hostButton = new JButton("Host");
		hostButton.addActionListener(this);
		gbc.gridx = 2; 
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		add(hostButton, gbc);
		
		joinAddress = new JTextField(""); // for testing: 192.168.1.50:5309
		joinAddress.setPreferredSize(new Dimension(200, 20));
		joinAddress.setHorizontalAlignment(JTextField.CENTER);
		gbc.gridx = 0; 
		gbc.gridy = 1;
		gbc.gridwidth = 2;
		add(joinAddress, gbc);
		
		JButton joinButton = new JButton("Join");
		joinButton.addActionListener(this);
		gbc.gridx = 2; 
		gbc.gridy = 1;
		gbc.gridwidth = 1;
		add(joinButton, gbc);
		
		pack();
	}

	/**
	 * Run when one of the buttons is clicked.
	 * @param event Event parameters (determine which button was clicked).
	 */
	@Override 
	public void actionPerformed(ActionEvent event) {
		String buttonText = ((JButton) event.getSource()).getText();
		
		if ("Host".equals(buttonText)) {
			try {
				DocumentManager.NewShared();
				setVisible(false);
			} catch(Exception ex) {
				JOptionPane.showMessageDialog(this, "Cannot host server:\n" + ex.getMessage(), "Cannot host", JOptionPane.OK_OPTION);
			}
		} else if ("Join".equals(buttonText)) {
			try {
				DocumentManager.OpenShared(joinAddress.getText());
				setVisible(false);
			} catch(Exception ex) {
				JOptionPane.showMessageDialog(this, "Cannot connect to server: " + joinAddress.getText(), "Cannot join", JOptionPane.OK_OPTION);
			}
		}		
	}
}
