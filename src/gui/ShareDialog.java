package gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;
import java.util.*;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;

import util.errors.ErrorManager;

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
	static String MyKey = generateKey();
	
	static JPanel HostPanel = new JPanel();
	static JLabel HostStatus = new JLabel();
	
	static JPanel JoinPanel = new JPanel();
	static Map<String, InetAddress> AvailableHosts = new HashMap<String, InetAddress>();
	
	static InetAddress Group;
	static MulticastSocket Socket;
	
	/**
	 * Create the dialog. Only do this once.
	 */
	private ShareDialog() {
		dialog = new JDialog();
		dialog.setTitle("Share current document");
		dialog.setSize(300, 200);
		dialog.setLayout(new BorderLayout());
		dialog.setLocationByPlatform(true);
		dialog.setModal(true);
		dialog.setDefaultCloseOperation(JDialog.HIDE_ON_CLOSE);
		
		// Make the label for displaying a host key.
		HostPanel.setLayout(new BorderLayout());
		
		JLabel keyLabel = new JLabel(MyKey); 
		keyLabel.setBackground(Color.BLACK);
		keyLabel.setFont(new Font("Monospaced", Font.PLAIN, 32));
		keyLabel.setHorizontalAlignment(SwingConstants.CENTER);
		
		HostStatus.setText("Waiting for connections...");
		HostStatus.setHorizontalAlignment(SwingConstants.CENTER);
		
		HostPanel.add(HostStatus, BorderLayout.NORTH);
		HostPanel.add(keyLabel, BorderLayout.CENTER);

		// Make the panel for displaying available hosts.
		JoinPanel.setLayout(new BorderLayout());
		
		JoinPanel.add(new JLabel("Choose a host to connect to:"), BorderLayout.NORTH);
		
		JTable table = new JTable(new AbstractTableModel() {
			private static final long serialVersionUID = 1702947099675368863L;

			@Override
			public Object getValueAt(int row, int col) { return AvailableHosts.get(row); }
			
			@Override
			public void setValueAt(Object val, int row, int col) {}
			
			@Override
			public int getRowCount() { return AvailableHosts.size(); }
			
			@Override
			public int getColumnCount() { return 1; }
			
			@Override
			public String getColumnName(int col) { return "Host"; }
			
			@Override
			public boolean isCellEditable(int row, int col) { return false; }
		});
		JoinPanel.add(new JScrollPane(table));
		
		// Set up the networking features.
		boolean networkingEnabled = false;
		try {
			Group = InetAddress.getByName("228.96.62.28");
			Socket = new MulticastSocket(9662);
			Socket.joinGroup(Group);
			
			networkingEnabled = true;
		} catch (UnknownHostException e) {
			ErrorManager.logError("Unable to establish network: " + e.getMessage());
		} catch (IOException e) {
			ErrorManager.logError("Unable to establish network: " + e.getMessage());
		}
		
		Thread t = new Thread(new Runnable() {
			@Override
			public void run() {
				byte[] buffer = new byte[1024];
				DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
				
				while (true) {
					if (State == ShareDialogState.Join) {
						try {
							Socket.receive(packet);
							String data = new String(packet.getData());
							String[] parts = data.split("=");
							
							if (parts.length != 2)
								continue;
							
							String key = parts[0];
							InetAddress ip = InetAddress.getByName(parts[1]);
							
							AvailableHosts.put(key, ip);
							
						} catch (IOException e) {
							ErrorManager.logError("Error receiving packet: " + e.getMessage());
						}
					} else if (State == ShareDialogState.Host) {
						try {
							String toSend = (MyKey + "=" + InetAddress.getLocalHost().toString());
							
							packet.setData(toSend.getBytes());
							Socket.send(packet);
							
						} catch (UnknownHostException e) {
							ErrorManager.logError("Error sending packet: " + e.getMessage());
						} catch (IOException e) {
							ErrorManager.logError("Error sending packet: " + e.getMessage());
						}
					} else {
						try { Thread.sleep(1000); } catch(InterruptedException ex) {}
					}
				}
			}			
		});
		t.setDaemon(true);
		t.start();
		
		// Make buttons. Because everyone loves buttons.
		final JButton host = new JButton("Host");
		final JButton join = new JButton("Join");
		final JButton cancel = new JButton("Cancel");
		
		JPanel buttons = new JPanel();
		buttons.setLayout(new GridLayout(1, 3));
		dialog.add(buttons, BorderLayout.SOUTH);
		
			if (networkingEnabled) {
			
			// This computer will act as a host.
			host.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					if (State == ShareDialogState.Default) {
						host.setEnabled(false);
						join.setEnabled(false);
						
						dialog.add(HostPanel, BorderLayout.CENTER);
						
						dialog.repaint();
						
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
						AvailableHosts.clear();
						
						host.setEnabled(false);
						
						dialog.add(JoinPanel, BorderLayout.CENTER);
						
						dialog.repaint();
						
						State = ShareDialogState.Join;
					}
				}
			});
			buttons.add(join);
			
			// Either cancel the current host/join or close the dialog, depending on state.
			cancel.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent arg0) {
					if (State == ShareDialogState.Default)
						dialog.setVisible(false);
					else {
						host.setEnabled(true);
						join.setEnabled(true);
						
						if (State == ShareDialogState.Host)
							dialog.remove(HostPanel);
						else if (State == ShareDialogState.Join)
							dialog.remove(JoinPanel);
						
						dialog.repaint();
						
						State = ShareDialogState.Default;
					}
				}
			});
			buttons.add(cancel);
		}
		
		// Networking is disabled.
		else {
			host.setEnabled(false);
			join.setEnabled(false);
			
			JLabel error = new JLabel("Unabled to establish network connection.");
			error.setHorizontalAlignment(SwingConstants.CENTER);
			dialog.add(error, BorderLayout.CENTER);
		}
	}
	
	/**
	 * Generate a unique, easy to type key for this machine.
	 * @return
	 */
	private static String generateKey() {
		Random r = new Random();
		char[] vowels = new char[]{'a', 'e', 'i', 'o', 'u'};
		
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < 4; i++) {
			sb.append((char) (97 + r.nextInt(26)));
			sb.append(vowels[r.nextInt(5)]);
		}
		
		return sb.toString();
	}

	/**
	 * Show the dialog.
	 */
	public static void show() {
		if (dialog == null) new ShareDialog();
		dialog.setVisible(true);
	}
}
