package gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.net.*;

import javax.swing.*;

import util.errors.ErrorManager;
import util.networking.NetworkListener;
import util.networking.NetworkManager;

/**
 * Possible states of this dialog.
 */
enum ShareDialogState {
	Default,
	Host,
	Join
}

/**
 * A dialog for allowing users to share the current SchemeTextArea. 
 */
public class ShareDialog {
	static JDialog dialog;
	static ShareDialogState State = ShareDialogState.Default;

	static JPanel HostPanel = new JPanel();
	static JTextField HostStatus = new JTextField();
	
	static JPanel JoinPanel = new JPanel();
	static JTextField JoinIP = new JTextField();
	
	/**
	 * Create the dialog. Only do this once.
	 * @throws UnknownHostException If networking is disabled.
	 */
	private ShareDialog() throws UnknownHostException {
		dialog = new JDialog();
		dialog.setTitle("Share current document");
		dialog.setSize(300, 200);
		dialog.setLayout(new BorderLayout());
		dialog.setLocationByPlatform(true);
		dialog.setModal(true);
		dialog.setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		
		// Make the label for displaying a host key.
		HostPanel.setLayout(new BorderLayout());
		
		JLabel keyLabel = new JLabel(InetAddress.getLocalHost().getHostAddress()); 
		keyLabel.setBackground(Color.BLACK);
		keyLabel.setFont(new Font("Monospaced", Font.PLAIN, 28));
		keyLabel.setHorizontalAlignment(SwingConstants.CENTER);
		
		HostStatus.setEditable(false);
		HostStatus.setText("Waiting for connections...");
		HostStatus.setHorizontalAlignment(SwingConstants.CENTER);
		
		HostPanel.add(HostStatus, BorderLayout.NORTH);
		HostPanel.add(keyLabel, BorderLayout.CENTER);

		// Make the panel for displaying available hosts.
		JoinPanel.setLayout(new BorderLayout());
		JoinIP.setFont(new Font("Monospaced", Font.PLAIN, 28));
		JoinPanel.add(new JLabel("Choose a host to connect to:"), BorderLayout.NORTH);
		JoinPanel.add(JoinIP, BorderLayout.CENTER);
		
		// Make buttons. Because everyone loves buttons.
		final JButton host = new JButton("Host");
		final JButton join = new JButton("Join");
		final JButton cancel = new JButton("Cancel");
		
		JPanel buttons = new JPanel();
		buttons.setLayout(new GridLayout(1, 3));
		dialog.add(buttons, BorderLayout.SOUTH);
		
		// Listen for connections.
		NetworkManager.addNetworkListener(new NetworkListener() {
			@Override
			public void onReceive(String content) {}
			
			@Override
			public void onConnection(Socket client) {
				if ("Waiting for connections...".equals(HostStatus.getText()))
					HostStatus.setText("Clients connected:");
				HostStatus.setText(HostStatus.getText() + " " + client.getInetAddress().getHostAddress());
			}
		});
			
		// This computer will act as a host.
		host.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (State == ShareDialogState.Default) {
					host.setEnabled(false);
					join.setEnabled(false);
					
					NetworkManager.host();
					
					dialog.add(HostPanel, BorderLayout.CENTER);
					dialog.validate();
					
					State = ShareDialogState.Host;
				}
			}
		});
		buttons.add(host);
		
		// This computer will join another machine.
		join.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				if (State == ShareDialogState.Default) {
					host.setEnabled(false);

					dialog.add(JoinPanel, BorderLayout.CENTER);
					dialog.validate();
					
					State = ShareDialogState.Join;
				} else if (State == ShareDialogState.Join) {
					NetworkManager.join(JoinIP.getText());
				}
			}
		});
		buttons.add(join);
		
		// Either cancel the current host/join or close the dialog, depending on state.
		cancel.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				host.setEnabled(true);
				join.setEnabled(true);
				
				if (State == ShareDialogState.Host)
					dialog.remove(HostPanel);
				else if (State == ShareDialogState.Join)
					dialog.remove(JoinPanel);
				
				State = ShareDialogState.Default;
				dialog.setVisible(false);
			}
		});
		buttons.add(cancel);
	}

	/**
	 * Show the dialog.
	 */
	public static void show() {
		if (dialog == null) {
			try {
				new ShareDialog();
			} catch (UnknownHostException e) {
				ErrorManager.logError("Unable to initialize networking.");
			}
		}
		
		dialog.setVisible(true);
	}
}
