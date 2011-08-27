package globals;

import gnu.mapping.*;
import util.KawaWrap;

public class Treeitude extends Globals {
	/**
	 * Add methods related to (random n)
	 * 
	 * @param kawa The interpreter to add them to.
	 * @throws Throwable If anything breaks while adding them.
	 */
	@Override
	public void addMethods(KawaWrap kawa) throws Throwable {
		class Tree {
			Object Value;
			Tree Left;
			Tree Right;
			
			public Tree() {
			}
			
			public Tree(Object value) {
				Value = value;
				Left = new Tree();
				Right = new Tree();
			}
			
			public Tree(Object value, Tree left, Tree right) {
				Value = value;
				Left = left;
				Right = right;
			}
			
			public String toString() {
				if (Value == null) return "[empty-tree]";
				else if (Left.Value == null && Right.Value == null) return "[leaf " + Value + "]";
				else return "[tree " + Value + " " + Left + " " + Right + "]";
			}
		}
		
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
	}
}
