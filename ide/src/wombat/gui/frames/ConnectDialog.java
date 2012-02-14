package wombat.gui.frames;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import wombat.util.files.DocumentManager;

public class ConnectDialog extends JDialog implements ActionListener {
	private static final long serialVersionUID = 4997826705741440876L;

	static ConnectDialog me;
	static String hostPrefix;
	
	JTextArea joinAddress;
	
	/**
	 * Create a new connection dialog. 
	 * @param text
	 */
	public ConnectDialog(MainFrame parent) {
		super(parent, true);
		
		setSize(400, 300);
		setBackground(Color.RED);
		setLocationByPlatform(true);
		setTitle("Connect...");
		setResizable(false);
		setLayout(new GridLayout(4, 1));

		JButton hostButton = new JButton("Host");
		hostButton.addActionListener(this);
		add(hostButton);
		
		add(new JLabel());
		
		joinAddress = new JTextArea("192.168.1.50:5309");
		add(joinAddress);
		
		JButton joinButton = new JButton("Join");
		joinButton.addActionListener(this);
		add(joinButton);
	}

	/**
	 * Run when one of the buttons is clicked.
	 */
	@Override public void actionPerformed(ActionEvent e) {
		String buttonText = ((JButton) e.getSource()).getText();
		
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
