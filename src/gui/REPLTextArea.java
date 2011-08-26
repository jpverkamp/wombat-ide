package gui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.util.Stack;

import javax.swing.AbstractAction;
import javax.swing.KeyStroke;
import javax.swing.text.BadLocationException;

/**
 * Run commands.
 */
public class REPLTextArea extends SchemeTextArea {
	private static final long serialVersionUID = 8753865168892947915L;
	
	/**
	 * Create a new REPL area.
	 */
	public REPLTextArea() {
		setPreferredSize(new Dimension(100, 100));
        code.getInputMap().put(
                KeyStroke.getKeyStroke("ENTER"),
                new AbstractAction() {
                    private static final long serialVersionUID = 723647997099071931L;

					public void actionPerformed(ActionEvent e) {
                        Stack<Character> brackets = new Stack<Character>();
                        for (char c : getText().toCharArray()) {
                            if (c == '(') brackets.push(')');
                            else if (c == '[') brackets.push(']');
                            else if (c == ')' || c == ']')
                                if (!brackets.empty() && brackets.peek() == c)
                                    brackets.pop();
                                else
                                    return;
                        }

                        if (brackets.empty()) {
                            MainFrame.me().doCommand(getText());
                            setText("");
                        } else {
                            try {
                                code.getDocument().insertString(code.getCaretPosition(), SchemeTextArea.NL, null);
                            } catch (BadLocationException ble) {
                                System.err.println("badwolf");
                            }
                            tab();
                        }
                    }
                }
        );
	}

}
