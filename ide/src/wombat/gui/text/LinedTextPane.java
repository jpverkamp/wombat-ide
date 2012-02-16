package wombat.gui.text;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;

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
	 * @param sta
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

        getInputMap().put(KeyStroke.getKeyStroke("TAB"), new wombat.gui.actions.Tab());
        getInputMap().put(KeyStroke.getKeyStroke("ENTER"), new wombat.gui.actions.Return());
        
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
	
	@Override public void paint(Graphics go) {
    	super.paint(go);
    	
    	Graphics2D g = (Graphics2D) go;
    	
    	int width = 2 + 80 * g.getFontMetrics(new Font("Monospaced", Font.PLAIN, Options.FontSize)).charWidth(' '); 
    	
    	g.setColor(Color.LIGHT_GRAY);
    	g.drawLine(width, 0, width, getHeight() + 10);
	}
}

