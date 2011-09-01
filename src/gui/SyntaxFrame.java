package gui;

import java.awt.BorderLayout;
import java.util.*;
import javax.swing.*;

import javax.swing.table.AbstractTableModel;

public class SyntaxFrame extends JFrame {
	private static final long serialVersionUID = 6110006176994509078L;
	
	// Pairs of name and indentation.
	class SyntaxPair {
		public String Name;
		public int Indentation;
		
		public SyntaxPair(String name, int indentation) {
			Name = name;
			Indentation = indentation;
		}
	}
	List<SyntaxPair> syntaxValues = new ArrayList<SyntaxPair>();
	
	/**
	 * Create a new syntax frame.
	 */
	public SyntaxFrame() {
		JTable table = new JTable(new AbstractTableModel() {
			private static final long serialVersionUID = -8298784246564975586L;

			@Override
			public Object getValueAt(int row, int col) {
				if (row >= syntaxValues.size())
					return null;
				else 
					if (col == 0) 
						return syntaxValues.get(row).Name;
					else
						return syntaxValues.get(row).Indentation;
			}
			
			@Override
			public void setValueAt(Object val, int row, int col) {
				if (row == syntaxValues.size())
					if (col == 0)
						syntaxValues.add(new SyntaxPair((String) val, 2));
					else
						syntaxValues.add(new SyntaxPair(null, Integer.parseInt((String) val)));
				else
					if (col == 0)
						syntaxValues.get(row).Name = (String) val;
					else
						syntaxValues.get(row).Indentation = Integer.parseInt((String) val);
			}
			
			@Override
			public int getRowCount() { return syntaxValues.size() + 1; }
			
			@Override
			public int getColumnCount() { return 2; }
			
			@Override
			public String getColumnName(int col) { return (col == 0 ? "Keyword" : "Indentation"); }
			
			@Override
			public boolean isCellEditable(int row, int col) { return true; }
		});
		
		setTitle("Syntax options");
		setSize(400, 400);
		setLayout(new BorderLayout());
		setLocationByPlatform(true);
		add(new JScrollPane(table));
		setVisible(true);
	}
}
