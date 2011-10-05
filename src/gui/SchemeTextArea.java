package gui;

import javax.swing.*;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.*;
import javax.swing.undo.UndoManager;

import wombat.Options;

import java.awt.*;
import java.io.*;
import java.util.*;

/**
 * Text area specialized for Scheme (Woo!)
 */
public class SchemeTextArea extends JPanel {
	private static final long serialVersionUID = -5290625425897085428L;

	public File myFile;
	public net.infonode.docking.View myView;
	public JTextPane code;
    public static String NL = "\n"; //System.getProperty("line.separator");
    public int SavedHash;
    public UndoManager Undo = new UndoManager();
    
    /**
     * Create a new Scheme text area.
     */
    public SchemeTextArea() {
        super();
        setLayout(new BorderLayout());

        code = new JTextPane() {
			private static final long serialVersionUID = 2523699493531510651L;

			@Override
        	public void paint(Graphics go) {
            	super.paint(go);
            	
            	Graphics2D g = (Graphics2D) go;
            	
            	
            	int width = 2 + 80 * g.getFontMetrics(new Font("Monospaced", Font.PLAIN, Options.FontSize)).charWidth(' '); 
            	
            	g.setColor(Color.LIGHT_GRAY);
            	g.drawLine(width, 0, width, getHeight() + 10);
        	}
        };
        final JScrollPane cs = new JScrollPane(code);
        add(cs);
        
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 100; i++)
        	sb.append(i + "\n");
        
        final SchemeDocument doc = new SchemeDocument();
        final StyledEditorKit sek = new StyledEditorKit() {
			private static final long serialVersionUID = 8558935103754214456L;

			public Document createDefaultDocument() {
                return doc;
            }
        };

        code.setFont(new Font("Monospaced", Font.PLAIN, Options.FontSize));
        code.setEditorKitForContentType("text/scheme", sek);
        code.setContentType("text/scheme");
        code.setEditorKit(sek);
        code.setDocument(doc);

        code.getInputMap().put(KeyStroke.getKeyStroke("TAB"), new actions.Tab());
        code.getInputMap().put(KeyStroke.getKeyStroke("ENTER"), new actions.Return());
        
        for (String name : new String[]{
        		"New", "Open", "Save", "Save as", "Close", "Exit", 
        		"Cut", "Copy", "Paste", "Undo", "Redo", 
        		"Run", "Format"}) {
        	JMenuItem item = MenuManager.itemForName(name);
        	code.getInputMap().put(item.getAccelerator(), item.getAction());
        }
        
        // Bracket highlighting.
        code.addCaretListener(new BracketMatcher(this));
        
        // Undo/redo
        

        // Listen for undo and redo events
        doc.addUndoableEditListener(new UndoableEditListener() {
            public void undoableEditHappened(UndoableEditEvent evt) {
                Undo.addEdit(evt.getEdit());
            }
        });
    }

    /**
     * Create a new Scheme text area with content.
     *
     * @param text Content.
     * @throws FileNotFoundException, IOException 
     */
    public SchemeTextArea(File file) throws FileNotFoundException, IOException {
        this();
        myFile = file;
        load();
    }
    
    /**
     * Load the document from it's file (throws an exception if the file hasn't been set).
     * @throws FileNotFoundException, IOException If we can't save based on a file error.
     */
    public void load() throws FileNotFoundException, IOException {
    	if (myFile == null) throw new FileNotFoundException("No file set");
    	
    	Scanner scanner = new Scanner(myFile);
        StringBuilder content = new StringBuilder();
        String NL = "\n"; //System.getProperty("line.separator");

        while (scanner.hasNextLine()) {
            content.append(scanner.nextLine());
            content.append(NL);
        }
        
        setText(content.toString());
        SavedHash = getText().hashCode();
    }
    
    /**
     * Save the document to its file (throws an exception if the file hasn't been set).
     * @throws FileNotFoundException, IOException If it doesn't work.
     */
    public void save() throws FileNotFoundException, IOException {
    	if (myFile == null) throw new FileNotFoundException("No file set");
    	
    	Writer out = new OutputStreamWriter(new FileOutputStream(myFile));
        out.write(getText());
        out.flush();
        out.close();
        
        SavedHash = getText().hashCode();
    }
    
    /**
     * Is the document dirty?
     * @return If it has changed since the last time.
     */
    public boolean isDirty() {
    	return getText().hashCode() != SavedHash;
    }
    
    /**
     * Perform a tab at the current position.
     */
    public void tab() {
    	
    }

    /**
     * Format the document.
     */
    public void format() {
        int next = -1;
        int eof = getText().indexOf("#!eof");
        if (eof == -1) eof = getText().length();
        
        while (true) {
            next = getText().indexOf(NL, next + 1) + NL.length();

            if (next > 0 && next < eof) {
                try {
                    code.setCaretPosition(next);
                    tab();
                } catch (IllegalArgumentException iae) {
                    // Means there's extra lines. Just ignore it.
                }
            } else
                break;
        }
    }

    /**
     * Is the text area empty?
     *
     * @return True/false
     */
    public boolean isEmpty() {
        return getText().length() == 0;
    }

    /**
     * Set the file that this code is associated with.
     *
     * @param f The file.
     */
    public void setFile(File f) {
        myFile = f;
    }

    /**
     * Get the file that this code is associated with (might be null).
     *
     * @return The file.
     */
    public File getFile() {
        return myFile;
    }

    /**
     * Get the code.
     *
     * @return The code.
     */
    public String getText() {
        return code.getText();
    }

    /**
     * Set the code.
     */
    public void setText(String text) {
        code.setText(text);
    }

    /**
     * Append text to the end of the code area.
     *
     * @param text Text to append.
     */
    public void append(String text) {
        setText(getText() + text);
    }

    /**
     * Update the font.
     */
	public void updateFont() {
		// For the document to refresh.
		try {
			((SchemeDocument) code.getDocument()).processChangedLines(0, getText().length());
		} catch (BadLocationException e) {
		}
	}
}
