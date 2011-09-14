package gui;

import java.awt.Frame;
import java.io.IOException;
import java.io.Writer;

import javax.swing.JFrame;

import util.KawaWrap;

import gnu.mapping.OutPort;

public class SchemePrinter extends OutPort {
	String Name;
	SchemeTextArea WriteTo;
	
	/**
	 * Create a new SchemePrinter.
	 */
	public SchemePrinter(final String name, final SchemeTextArea writeTo) {
		super(new Writer() {
			@Override
			public void close() throws IOException {}

			@Override
			public void flush() throws IOException {}

			@Override
			public void write(char[] cbuf, int off, int len) throws IOException {
				writeTo.append(new String(cbuf, off, len));
				
				for (Frame frame : JFrame.getFrames())
					if (frame instanceof MainFrame)
						if (frame.isVisible()) // make sure that it doesn't try to keep writing once everything is closing down
							((MainFrame) frame).showView(name);
			}
		}, true, true);
		
		Name = name;
		WriteTo = writeTo;
	}
	
	/**
	 * Write an object.
	 * @param v
	 */
	public void print(Object v) {
		if (v instanceof String || v instanceof gnu.lists.FString)
			WriteTo.append(v.toString());
		else
			WriteTo.append(KawaWrap.formatObject(v));
		
		for (Frame frame : JFrame.getFrames())
			if (frame instanceof MainFrame)
				((MainFrame) frame).showView(Name);
	}
}
