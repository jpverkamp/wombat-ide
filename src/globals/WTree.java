package globals;

import java.awt.*;
import java.awt.image.BufferedImage;

import javax.swing.*;

import gnu.mapping.*;
import util.KawaWrap;

public class WTree extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(KawaWrap kawa) throws Throwable {
		kawa.bind(new Procedure3("tree") {
			public Object apply3(Object value, Object left, Object right) throws Throwable {
				if (!(left instanceof Tree)) throw new IllegalArgumentException("Error in tree: " + left + " is not a valid subtree.");
				if (!(right instanceof Tree)) throw new IllegalArgumentException("Error in tree: " + right + " is not a valid subtree.");
				return new Tree(value, (Tree) left, (Tree) right);
			}
		});
		
		kawa.bind(new Procedure1("tree?") {
			public Object apply1(Object tr) {
				return (tr instanceof Tree);
			}
		});
		
		kawa.bind(new Procedure1("leaf") {
			public Object apply1(Object value) {
				return new Tree(value);
			}
		});
		
		kawa.bind(new Procedure1("leaf?") {
			public Object apply1(Object tr) {
				return ((tr instanceof Tree) && ((Tree) tr).Left == null && ((Tree) tr).Right == null);
			}
		});
		
		kawa.bind(new Procedure0("empty-tree") {
			public Object apply0() {
				return new Tree();
			}
		});
		
		kawa.bind(new Procedure1("empty-tree?") {
			public Object apply1(Object tr) {
				return ((tr instanceof Tree) && ((Tree) tr).Value == null);
			}
		});
		
		kawa.bind(new Procedure1("left-subtree") {
			public Object apply1(Object tr) {
				if (!(tr instanceof Tree)) throw new IllegalArgumentException("Error in left-subtree: " + tr + " is not a tree.");
				if (((Tree) tr).Left == null) throw new IllegalArgumentException("Error in left-subtree: " + tr + " does not have a left subtree.");
				return ((Tree) tr).Left;
			}
		});
		
		kawa.bind(new Procedure1("right-subtree") {
			public Object apply1(Object tr) {
				if (!(tr instanceof Tree)) throw new IllegalArgumentException("Error in right-subtree: " + tr + " is not a tree.");
				if (((Tree) tr).Right == null) throw new IllegalArgumentException("Error in right-subtree: " + tr + " does not have a right subtree.");
				return ((Tree) tr).Right;
			}
		});

		kawa.bind(new Procedure1("root-value") {
			public Object apply1(Object tr) {
				if (!(tr instanceof Tree)) throw new IllegalArgumentException("Error in root-value: " + tr + " is not a tree.");
				if (((Tree) tr).Value == null) throw new IllegalArgumentException("Error in root-value: " + tr + " is an empty tree.");
				return ((Tree) tr).Value;
			}
		});
		
		kawa.bind(new Procedure1("draw-tree") {
			public Object apply1(Object tr) {
				if (!(tr instanceof Tree)) throw new IllegalArgumentException("Error in draw-tree: " + tr + " is not a tree.");
				
				JFrame treeFrame = new JFrame("draw-tree");
				treeFrame.setSize(400, 400);
				treeFrame.setLayout(new BorderLayout());
				treeFrame.add(new JLabel(new ImageIcon(((Tree) tr).drawTree(400, 400))));
				treeFrame.setVisible(true);
				
				return null;
			}
		});
	}
}

/**
 * A simple tree.
 */
class Tree {
	Object Value;
	Tree Left;
	Tree Right;
	
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
	public Image drawTree(int width, int height) {
		Image treeImage = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
		
		Graphics2D g = (Graphics2D) treeImage.getGraphics();
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, width, height);
		
		int h = height();
		
		
		
		
		g.setColor(Color.RED);
		g.drawLine(0, 0, width, height);
		
		g.drawString("The height is: " + h, height / 2, 100);
		
		return treeImage;
	}
	

	/**
	 * Calculate the height of the tree (empty tree is 0, leaf is 1).
	 * @return The height of the tree.
	 */
	private int height() {
		if (Value == null) return 0;
		else return 1 + Math.max(Left.height(), Right.height());
	}
}
