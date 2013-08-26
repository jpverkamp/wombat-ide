/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.frames;

import java.awt.Frame;

import javax.swing.GroupLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.LayoutStyle;
import javax.swing.WindowConstants;

/**
 * Dialog to find and replace text.
 */
public class FindReplaceDialog extends JDialog {
	private static final long serialVersionUID = -4069253877583476204L;

	// Text to find and replace.
	private JTextPane text;
	
	// Search position.
	private int currentPos = -1;
	private int pos = -1;

	// Text to find / replace it with.
	private JTextField findTextField;
	private JTextField replaceTextField;

	/**
	 * Create a new find/replace dialog.
	 * @param parent The parent frame of this dialog.
	 * @param text The text pane to replace text in.
	 */
	public FindReplaceDialog(final Frame parent, final JTextPane textPane) {
		super(parent, false);
		
		this.text = textPane;
		this.currentPos = this.text.getCaretPosition();
		
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		setTitle("Find/Replace");

		replaceTextField = new JTextField();
		findTextField = new JTextField();

		// So we can access it in the nested event listener. 
		final FindReplaceDialog me = this;
		
		// Button to find the next matching bit of text.
		final JButton findButton = new JButton("Find Next");
		findButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent event) {
				System.err.println("Find starting at " + currentPos); // TODO: DEBUG
				
				// Nothing to search for.
				if (findTextField.getText().isEmpty()) return;

				// Get the search term and sanity check.
				String context = text.getText();
				if (currentPos > context.length()) return;
				String wordToFind = findTextField.getText();
				pos = context.indexOf(wordToFind, currentPos);
				int startPos = context.indexOf(wordToFind, 0);
				
				// Fix to start at the beginning if we didn't find
				if (pos == -1 && startPos != -1) pos = startPos;
				
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

		// Button to replace the current instance with the replacement text.
		final JButton replaceButton = new JButton("Replace");
		replaceButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent event) {
				if (!findTextField.getText().isEmpty() && text.getSelectedText().equals(findTextField.getText()) && pos != -1) { 
					text.replaceSelection(replaceTextField.getText());
					findButton.doClick();
				}
			}
		});

		// Replace all instances of the target text with the replacement.
		final JButton replaceAllButton = new JButton("Replace All");
		replaceAllButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent event) {
				text.setText(text.getText().replace(findTextField.getText(), replaceTextField.getText()));
			}
		});

		final JLabel replaceLabel = new JLabel("Replace with: ");
		final JLabel findLabel = new JLabel("Find: ");

		// Layout the group content.
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