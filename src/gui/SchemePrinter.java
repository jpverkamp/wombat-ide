package gui;

import java.awt.Frame;
import java.io.StringWriter;

import javax.swing.JFrame;

import util.KawaWrap;

import gnu.mapping.OutPort;

public class SchemePrinter extends OutPort {
	String Name;
	SchemeTextArea WriteTo;
	
	StringBuilder Buffer;
	boolean HasContent;
	
	/**
	 * Create a new SchemePrinter.
	 */
	public SchemePrinter(final String name, final SchemeTextArea writeTo) {
		super(new StringWriter());
		
		Name = name;
		WriteTo = writeTo;
		
		Buffer = new StringBuilder();
		HasContent = false;
	}
	
	/**
	 * Write an object.
	 * @param v
	 */
	public void print(Object v) {
		
		if (v instanceof String || v instanceof gnu.lists.FString) {
			Buffer.append(v.toString());
		} else {
			Buffer.append(KawaWrap.formatObject(v));
		}
		HasContent = true;
		
	}

	public void showContent() {
		WriteTo.append(Buffer.toString());
		Buffer.delete(0, Buffer.length());
		HasContent = false;

		for (Frame frame : JFrame.getFrames())
			if (frame instanceof MainFrame)
				((MainFrame) frame).showView(Name);
	}
}
