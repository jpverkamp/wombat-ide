package gui;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import javax.swing.event.CaretListener;

import javax.swing.event.CaretEvent;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.StyleConstants;
import javax.swing.text.Utilities;

import util.errors.ErrorManager;

public class BracketMatcher implements CaretListener {
	SchemeTextArea textArea;
	static boolean disabled = false;
	List<Object> activeTags = new ArrayList<Object>();
	
	public BracketMatcher(SchemeTextArea text) {
		textArea = text;
	}
	
	public void caretUpdate(CaretEvent e) {
		try {
			int caretPos = textArea.code.getCaretPosition();
			int rowNum = (caretPos == 0) ? 1 : 0;
			for (int offset = caretPos; offset > 0;) {
			    offset = Utilities.getRowStart(textArea.code, offset) - 1;
			    rowNum++;
			}

			int offset = Utilities.getRowStart(textArea.code, caretPos);
			int colNum = caretPos - offset + 1;
			
			MainFrame.RowColumn.setText(rowNum + ":" + colNum);
		} catch(BadLocationException ex) {
			MainFrame.RowColumn.setText("row:column");
		}
		
		if (disabled)
			return;
		
		try {
			Highlighter h = textArea.code.getHighlighter();
			for (Object tag : activeTags)
				h.removeHighlight(tag);
			activeTags.clear();

			int pos = e.getDot() - 1;
        
            if (pos >= 0 && pos < textArea.getText().length() && "()[]".contains(textArea.code.getDocument().getText(pos, 1))) {
		// skip character literals
		if (pos >= 2 && "#\\".equals(textArea.code.getDocument().getText(pos - 2, 2)))
		    return;

                // Find the matching bracket.
                String text = textArea.code.getDocument().getText(0, textArea.code.getDocument().getLength());
                
                char orig, c;
                orig = c = text.charAt(pos);
                int matchPos, d = ((orig == '(' || orig == '[') ? 1 : -1);
                Stack<Character> brackets = new Stack<Character>();
                
                int index;
                boolean foundMatch = false;
                for (matchPos = pos; matchPos >= 0 && matchPos < text.length(); matchPos += d) {
		    if (d < 0) {
			    index = text.lastIndexOf(';', matchPos);
	                    if (index >= 0 && text.lastIndexOf('\n', matchPos) < index) {
	                    	matchPos = index;
	                    	continue;
	                    }
	                    
	                    index = text.lastIndexOf("|#", matchPos);
	                    if (index >= 0 && text.lastIndexOf('\n', matchPos) < index) {
	                    	matchPos = text.lastIndexOf("#|", index);
	                    	continue;
	                    }
                	} else {
                		index = text.indexOf(';', matchPos);
	                    if (index >= 0 && text.indexOf('\n', matchPos) < index) {
	                    	matchPos = index;
	                    	continue;
	                    }
	                    
	                    index = text.indexOf("#|", matchPos);
	                    if (index >= 0 && text.indexOf('\n', matchPos) < index) {
	                    	matchPos = text.indexOf("|#", index);
	                    	continue;
	                    }
                	}
                	
                	c = text.charAt(matchPos);

			// ignore character literals
			if (matchPos >= 2 && "#\\".equals(text.substring(matchPos - 2, matchPos)))
			    continue;
                    
                    if (!brackets.isEmpty() && brackets.peek() == c) {
                        brackets.pop();
                        if (brackets.isEmpty()) {
                            foundMatch = true;
                            break;
                        }
                    } else if (!brackets.isEmpty() &&
                    		((brackets.peek() == '(' && c == '[') ||
            				 (brackets.peek() == '[' && c == '(') ||
            				 (brackets.peek() == ')' && c == ']') ||
            				 (brackets.peek() == ']' && c == ')'))) {
                    	foundMatch = false;
                    	break;
                    }
                    
                    else if (c == '(') brackets.push(')');
                    else if (c == ')') brackets.push('(');
                    else if (c == '[') brackets.push(']');
                    else if (c == ']') brackets.push('[');
                }

                // Highlight it.
                Highlighter.HighlightPainter hp = new DefaultHighlighter.DefaultHighlightPainter(
                        StyleConstants.getForeground(SchemeDocument.attributes.get(foundMatch ? "bracket" : "invalid-bracket"))
                );

                try {
                    activeTags.add(h.addHighlight(pos, pos + 1, hp));
                    activeTags.add(h.addHighlight(matchPos, matchPos + 1, hp));
                } catch (BadLocationException ble) {
                }
            }
        } catch (BadLocationException ex) {
			
        } catch (Exception ex) {
        	ErrorManager.logError("Unable to match paranthesis: " + ex.getMessage());
        	disabled = true;
        }
    }

}
