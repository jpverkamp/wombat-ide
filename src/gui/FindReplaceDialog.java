package gui;

import javax.swing.*;
import java.awt.Frame;

import javax.swing.GroupLayout;

/**
 * Dialog to find and replace text.
 */
public class FindReplaceDialog extends JDialog {
	private static final long serialVersionUID = -4069253877583476204L;

	private JTextPane text;
	
	private int currentPos = 0;
	private int pos = -1;

	private JTextField findTextField;
	private JTextField replaceTextField;

	/**
	 * Create a new find/replace dialog.
	 * @param parent The parent frame of this dialog.
	 * @param modal If the frame should be modal.
	 * @param text The text pane to replace.
	 */
	public FindReplaceDialog(Frame parent, boolean modal, JTextPane textPane) {
		super(parent, modal);
		
		final FindReplaceDialog me = this;
		this.text = textPane;

		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle("Find/Replace");

		replaceTextField = new JTextField();
		findTextField = new JTextField();

		JButton findButton = new JButton("Find Next");
		findButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				// Nothing to search for.
				if (findTextField.getText().isEmpty()) return;

				// Get the search term and sanity check.
				String context = text.getText();
				if (currentPos > context.length()) return;
				String wordToFind = findTextField.getText();
				pos = context.indexOf(wordToFind, currentPos);

				// Find the word, highlight if so.
				if (pos != -1) {
					text.setSelectionStart(pos);
					text.setSelectionEnd(pos + wordToFind.length());
					text.requestFocusInWindow();
					currentPos = pos + 1;
				} 
				
				// Oops. Couldn't find it.
				else {
					JOptionPane.showMessageDialog(me, "Could not find '" + wordToFind + "'");
				}
			}
		});

		JButton replaceButton = new JButton("Replace");
		replaceButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				if (!findTextField.getText().isEmpty() && text.getSelectedText().equals(findTextField.getText()) && pos != -1)
						text.replaceSelection(replaceTextField.getText());
			}
		});

		JButton replaceAllButton = new JButton("Replace All");
		replaceAllButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent evt) {
				text.setText(text.getText().replace(findTextField.getText(), replaceTextField.getText()));
			}
		});

		findTextField.addKeyListener(new java.awt.event.KeyAdapter() {
			public void keyReleased(java.awt.event.KeyEvent evt) {
				currentPos = 0;
			}
		});

		JLabel replaceLabel = new JLabel("Replace with: ");
		JLabel findLabel = new JLabel("Find: ");

		GroupLayout layout = new GroupLayout(getContentPane());
		getContentPane().setLayout(layout);
		layout.setHorizontalGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
			.addGroup(GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
				.addGap(14, 14, 14)
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
					.addComponent(replaceLabel)
					.addComponent(findLabel, GroupLayout.PREFERRED_SIZE, 42, GroupLayout.PREFERRED_SIZE))
				.addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING, false)
					.addComponent(findTextField)
					.addComponent(replaceTextField, GroupLayout.DEFAULT_SIZE, 196, Short.MAX_VALUE))
				.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
					.addGroup(layout.createSequentialGroup()
						.addGap(8, 8, 8)
						.addComponent(findButton, GroupLayout.DEFAULT_SIZE, 96, Short.MAX_VALUE))
					.addGroup(layout.createSequentialGroup()
						.addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
						.addGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
							.addComponent(replaceAllButton, GroupLayout.DEFAULT_SIZE, 98, Short.MAX_VALUE)
							.addComponent(replaceButton, GroupLayout.PREFERRED_SIZE, 98, GroupLayout.PREFERRED_SIZE))))
				.addContainerGap()));
	
		layout.setVerticalGroup(layout.createParallelGroup(GroupLayout.Alignment.LEADING)
			.addGroup(layout.createSequentialGroup()
			.addContainerGap()
			.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
				.addComponent(findTextField, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
				.addComponent(findButton)
				.addComponent(findLabel))
			.addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
			.addGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
				.addComponent(replaceLabel)
				.addComponent(replaceTextField, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
				.addComponent(replaceButton))
			.addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
			.addComponent(replaceAllButton)
			.addContainerGap()));
		
		pack();
	}
}