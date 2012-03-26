/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.gui.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import wombat.gui.frames.MenuManager;
import wombat.util.Options;

/**
 * Text pane with a right margin at 80 characters.
 */
public class LinedTextPane extends JTextPane {
	private static final long serialVersionUID = 2523699493531510651L;

	/**
	 * Create a new text pane.
	 * @param sta The text area that we're displaying.
	 */
	public LinedTextPane(final SchemeTextArea sta) {
		final SchemeDocument doc = new SchemeDocument();
        final StyledEditorKit sek = new StyledEditorKit() {
			private static final long serialVersionUID = 8558935103754214456L;

			public Document createDefaultDocument() {
                return doc;
            }
        };

        setFont(new Font("Monospaced", Font.PLAIN, Options.FontSize));
        setEditorKitForContentType("text/scheme", sek);
        setContentType("text/scheme");
        setEditorKit(sek);
        setDocument(doc);

        // Correctly set up tab and enter to indent for scheme code.
        getInputMap().put(KeyStroke.getKeyStroke("TAB"), new wombat.gui.actions.Tab());
        getInputMap().put(KeyStroke.getKeyStroke("ENTER"), new wombat.gui.actions.Return());
        
        // Use Ctrl/Cmd-L to insert lambda.
        getInputMap().put(KeyStroke.getKeyStroke(((Character) 'L').charValue(), Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), 
        		new AbstractAction() {
					private static final long serialVersionUID = 5003494780540838485L;
		
					@Override public void actionPerformed(ActionEvent e) {
						try {
							getDocument().insertString(getCaretPosition(), (Options.LambdaMode ? "\u03BB" : "lambda"), null);
						} catch (BadLocationException e1) {
							e1.printStackTrace();
						}
					}
				});
        
        // Copy the relevant keyboard shortcuts from the menu code to here for cut/copy/paste, etc.
        for (String name : new String[]{
        		"New", "Open", "Save", "Save as", "Close", "Exit", 
        		"Cut", "Copy", "Paste", "Undo", "Redo", 
        		"Run", "Format"}) {
        	JMenuItem item = MenuManager.itemForName(name);
        	getInputMap().put(item.getAccelerator(), item.getAction());
        }
        
        // Bracket highlighting.
        addCaretListener(new BracketMatcher(sta));

        // Listen for undo and redo events
        doc.addUndoableEditListener(new UndoableEditListener() {
            public void undoableEditHappened(UndoableEditEvent evt) {
            	if ("style change".equals(evt.getEdit().getPresentationName()))
            		return;
            	
            	sta.Undo.addEdit(evt.getEdit());
            }
        });
	}
	
	/**
	 * Draw the right margin.
	 * @param go The original graphics object.
	 */
	@Override
	public void paint(Graphics go) {
    	super.paint(go);
    	
    	Graphics2D g = (Graphics2D) go;
    	
    	int width = 2 + 80 * g.getFontMetrics(new Font("Monospaced", Font.PLAIN, Options.FontSize)).charWidth(' '); 
    	
    	g.setColor(Color.LIGHT_GRAY);
    	g.drawLine(width, 0, width, getHeight() + 10);
	}
}

