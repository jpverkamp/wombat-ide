/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.scheme.libraries;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.Scanner;

import javax.swing.*;
import javax.swing.border.BevelBorder;

/**
 * Java implementations of matrix code.
 */
public class MatrixAPI {
	/**
	 * Turn data into a matrix GUI.
	 * @param data Row number then column number, then row delimited data.
	 */
	public static void drawMatrix(String data) {
		// For the row and column indicies.
		class IndexLabel extends JLabel {
			private static final long serialVersionUID = -7749472183222490884L;
			public IndexLabel(String text) {
				super(text);
				setHorizontalAlignment(SwingConstants.CENTER);
			}
		}
		
		// For each of the actual cells.
		class DataLabel extends JLabel {
			private static final long serialVersionUID = -8188624620043747866L;
			public DataLabel(String text) {
				super(text);
				setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
				setHorizontalAlignment(SwingConstants.CENTER);
			}
		}
		
		// Scan one line at a time.
		Scanner s = new Scanner(data);
		
		// Get the size of the grid.
		int rows = Integer.parseInt(s.nextLine());
		int cols = Integer.parseInt(s.nextLine());
		
		// Create the grid, we'll put this in a JScrollPane later.
		JPanel grid = new JPanel();
		grid.setLayout(new GridLayout(rows + 1, cols + 1));
		grid.add(new JLabel());
		
		// Add an index across the top.
		for (int c = 0; c < cols; c++)
			grid.add(new IndexLabel("" + c));
		
		// Add each row number than a row of data.
		for (int r = 0; r < rows; r++) {
			grid.add(new IndexLabel("" + r));
			for (int c = 0; c < cols; c++)
				grid.add(new DataLabel(s.nextLine()));
		}
		
		// Clean up
		s.close();
		
		// Display the frame.
		JFrame matrixFrame = new JFrame("draw-matrix");
		matrixFrame.setSize(400, 400);
		matrixFrame.setLayout(new BorderLayout());
		matrixFrame.add(new JScrollPane(grid));
		matrixFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		matrixFrame.setVisible(true);
	}
}
