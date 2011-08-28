package gui;

import java.awt.Component;
import java.awt.Frame;
import java.io.IOException;
import java.io.Writer;

import javax.swing.JFrame;
import javax.swing.text.MutableAttributeSet;

import gnu.mapping.OutPort;

public class SchemePrinter extends OutPort {
	String Name;
	SchemeTextArea WriteTo;
	
	/**
	 * Create a new SchemePrinter.
	 */
	public SchemePrinter(String name, final SchemeTextArea writeTo) {
		super(new Writer() {
			@Override
			public void close() throws IOException {}

			@Override
			public void flush() throws IOException {}

			@Override
			public void write(char[] cbuf, int off, int len) throws IOException {
				writeTo.append(new String(cbuf, off, len));
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
		for (Frame frame : JFrame.getFrames())
			if (frame instanceof MainFrame)
				((MainFrame) frame).showView(Name);
				
		WriteTo.append(v.toString());
		System.out.println("Ready to print object: " + v + ":" + v.getClass().getName());
	}
	
	/**
	 * Write to a component. (what does this do?)
	 * @param c
	 */
	public void write(Component c) {
		System.out.println("Ready to print component: " + c);
	}
	
	/**
	 * Write a string.
	 * @param s
	 * @param style
	 */
	public void write(String s, MutableAttributeSet style) {
		System.out.println("Ready to print string: " + s);
	}
}
