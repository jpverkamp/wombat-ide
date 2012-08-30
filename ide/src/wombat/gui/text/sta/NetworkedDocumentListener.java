package wombat.gui.text.sta;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import wombat.util.Base64;

/**
 * Document listener which forwards changes to the network.
 */
public class NetworkedDocumentListener implements DocumentListener {
	SharedTextArea STA;
	boolean Suppress = false;
	Timer T;
	
	/**
	 * Create a new networked document listener.
	 * @param sta The shared text area.
	 */
	public NetworkedDocumentListener(SharedTextArea sta) {
		STA = sta;
		T = new Timer(1000, new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				int hash = STA.code.getText().hashCode();
				STA.CT.send(STA.makeMessage("check-sync", hash));
			}
		});
		T.setCoalesce(true);
		T.setRepeats(false);
		T.setDelay(Integer.MAX_VALUE);
		T.start();
	}
	
	/**
	 * When the document has changed. (Formatting, ignore these.)
	 */
	@Override public void changedUpdate(DocumentEvent event) { }

	/**
	 * When something is inserted.
	 */
	@Override public void insertUpdate(DocumentEvent event) {
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			String str = Base64.encodeBytes(STA.code.getText(off, len).getBytes("UTF-8"));
			
			STA.CT.send(STA.makeMessage("insert", off, len, str));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		T.restart();
	}

	/**
	 * When something is removed.
	 */
	@Override public void removeUpdate(DocumentEvent event) {
		if (Suppress) return;
		
		try {
			
			int off = event.getOffset();
			int len = event.getLength();
			
			STA.CT.send(STA.makeMessage("remove", off, len));
			
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		T.restart();
	}

	/**
	 * Get/set the suppressed value.
	 * @param b The new value.
	 */
	public void suppress(boolean b) {
		Suppress = b;
	}
}