package globals;

import java.awt.*;

import javax.swing.*;

import gnu.mapping.*;
import util.KawaWrap;
import util.Tree;

public class WTree extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(final KawaWrap kawa) throws Throwable {
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
				return ((tr instanceof Tree) && ((Tree) tr).isLeaf());
			}
		});
		
		kawa.bind(new Procedure0("empty-tree") {
			public Object apply0() {
				return new Tree();
			}
		});
		
		kawa.bind(new Procedure1("empty-tree?") {
			public Object apply1(Object tr) {
				return ((tr instanceof Tree) && ((Tree) tr).isEmpty());
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
				final Tree toDraw = (Tree) tr; 
				
				JFrame treeFrame = new JFrame("draw-tree") {
					private static final long serialVersionUID = -6723179597582742419L;

					@Override
					public void paint(Graphics g) {
						super.paint(g);
						toDraw.drawTree((Graphics2D) g, getWidth(), getHeight());
					}
				};
				treeFrame.setSize(400, 400);
				treeFrame.setLayout(new BorderLayout());
				treeFrame.setVisible(true);
				
				return null;
			}
		});
	}
}


