package util;

import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.math.BigInteger;

/**
 * A simple tree.
 */
public class Tree {
	public Object Value;
	public Tree Left;
	public Tree Right;
	
	int DisplayX = -1;
	int DisplayY = -1;
	
	/**
	 * Create an empty tree.
	 */
	public Tree() {
	}
	
	/**
	 * Create a leaf (no subtrees).
	 * @param value The leaf's value.
	 */
	public Tree(Object value) {
		Value = value;
		Left = new Tree();
		Right = new Tree();
	}
	
	/**
	 * Create a new leaf with possible subtrees.
	 * @param value The leaf's value.
	 * @param left Left subtree.
	 * @param right Right subtree.
	 */
	public Tree(Object value, Tree left, Tree right) {
		Value = value;
		Left = left;
		Right = right;
	}
	
	/**
	 * Convert a tree to a string.
	 */
	public String toString() {
		if (Value == null) return "[empty-tree]";
		else if (Left.Value == null && Right.Value == null) return "[leaf " + Value + "]";
		else return "[tree " + Value + " " + Left + " " + Right + "]";
	}
	
	/**
	 * Draw a tree.
	 * @return An image containing the tree.
	 */
	public void drawTree(Graphics2D g, int width, int height) {
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, width, height);
		
		g.setColor(Color.BLACK);
		int h = height();
		int w = new BigInteger("2").pow(h).intValue();
		
		setDisplay(width / 2, height / (h + 1), width / (w + 1), width - width / (w + 1),  height / (h + 1));
		drawLines(g);
		drawNodes(g);
	}
	
	/**
	 * Set up the display coordinates.
	 * @param x X-coordinate of this node.
	 * @param y Y-coordinate of this node.
	 * @param left Leftmost point we're allowed.
	 * @param right Rightmost point we're allowed.
 	 * @param skip Skip this between levels.
	 */
	private void setDisplay(int x, int y, int left, int right, int skip) {
		if (Value == null) return;
		
		DisplayX = x;
		DisplayY = y;
		
		int leftSize = 1 + Left.size();
		int rightSize = 1 + Right.size();
		double leftWeight = (double) leftSize / ((double) leftSize + (double) rightSize);
		int mid = left + (int) ((double) (right - left) * leftWeight);
		
		Left.setDisplay((left + mid) / 2, y + skip, left, mid, skip);
		Right.setDisplay((mid + right) / 2, y + skip, mid, right, skip);
	}
	
	
	/**
	 * Draw lines between this node and its children.
	 * @param g The graphics object to draw with.
	 */
	private void drawLines(Graphics2D g) {
		if (Value == null) return;
		if (Left.Value != null) g.drawLine(DisplayX, DisplayY, Left.DisplayX, Left.DisplayY);
		if (Right.Value != null) g.drawLine(DisplayX, DisplayY, Right.DisplayX, Right.DisplayY);
		
		Left.drawLines(g);
		Right.drawLines(g);
	}
	
	/**
	 * Draw this node and its children.
	 * @param g The graphics object to draw with.
	 */
	private void drawNodes(Graphics2D g) {
		if (Value == null) return;
		
		String s = Value.toString();
		FontMetrics fm = g.getFontMetrics();
		int w = fm.stringWidth(s);
		
		g.setColor(Color.WHITE);
		g.fillRect(DisplayX - w / 2 - 5, DisplayY - 10, w + 10, 20);
		g.setColor(Color.BLACK);
		g.drawRect(DisplayX - w / 2 - 5, DisplayY - 10, w + 10, 20);
		
		g.drawString(Value.toString(), DisplayX - fm.stringWidth(s) / 2, DisplayY + fm.getAscent() / 2);
		
		Left.drawNodes(g);
		Right.drawNodes(g);
	}
	
	/**
	 * Calculate the height of the tree (empty tree is 0, leaf is 1).
	 * @return The height of the tree.
	 */
	private int height() {
		if (Value == null) return 0;
		else return 1 + Math.max(Left.height(), Right.height());
	}
	
	/**
	 * Count this node plus all its children.
	 * @return The number of nodes.
	 */
	private int size() {
		if (Value == null) return 0;
		else return 1 + Left.size() + Right.size();
	}
}