package wombat.scheme;

import java.util.*;

import wombat.scheme.values.SchemeObject;
import wombat.scheme.values.SchemePair;

/**
 * Either an object or a list of objects.
 */
public class SExpression extends SchemeObject<Object> {
	private static final long serialVersionUID = 4890762867658098391L;

	SchemeObject<?> LiteralValue;
	List<SExpression> ListValue;
	
	int Line;
	int Column;
	
	SExpression () {
		super(null);
	}

	/**
	 * Set the line and column of this s-expression.
	 * @param line The line.
	 * @param column The column.
	 * @return This s-expression.
	 */
	public SExpression at(int line, int column) {
		Line = line;
		Column = column;
		
		if (isLiteral())
			LiteralValue.at(line, column);
		
		return this;
	}
	
	/**
	 * Create a new literal value.
	 * @param value The value to store.
	 * @return The new SExpression.
	 */
	public static SExpression literal(SchemeObject<?> value) {
		SExpression result = new SExpression();
		result.LiteralValue = value;
		return result;
	}
	
	/**
	 * Create a new s-expression list.
	 * @param values The nested s-expressions to store.
	 * @return The new SExpression.
	 */
	public static SExpression list(SExpression... values) {
		SExpression result = new SExpression();
		result.ListValue = new ArrayList<SExpression>(values.length);
		for (SExpression value : values)
			result.ListValue.add(value);
		return result;
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		if (isLiteral())
			return LiteralValue.toString();
		else if (isList()) 
			return Arrays.toString(ListValue.toArray());
		else
			return "#<broken s-expression>";
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		if (isLiteral())
			return LiteralValue.toString();
		else if (isList()) 
			return Arrays.toString(ListValue.toArray());
		else
			return "#<broken s-expression>";
	}
	
	/**
	 * Convert to a string for debuggin.
	 * @return That string.
	 */
	public String toString() {
		if (isLiteral())
			return LiteralValue.toString();
		else if (isList()) 
			return Arrays.toString(ListValue.toArray());
		else if (this instanceof Tag)
			return getClass().getName().replace("wombat.scheme.", "");
		else
			return "#<broken s-expression>";
	}
	
	
	/**
	 * Is this a literal value?
	 * @return True or false.
	 */
	public boolean isLiteral() {
		return LiteralValue != null;
	}
	
	/**
	 * Access the literal value.
	 * @return The value.
	 */
	public SchemeObject<?> getLiteral() {
		return LiteralValue;
	}
	
	/**
	 * Is this a list?
	 * @return True or false.
	 */
	public boolean isList() {
		return ListValue != null;
	}
	
	/**
	 * Access the list.
	 * @return The list.
	 */
	public List<SExpression> getList() {
		return ListValue;
	}

	/**
	 * Add another value to this s-expression's list.
	 * @param next The new value as a literal.
	 */
	public void add(SchemeObject<?> next) {
		if (next != null)
			ListValue.add(SExpression.literal(next));
	}
	
	/**
	 * Add another value to this s-expression's list.
	 * @param next The new value as an s-expression.
	 */
	public void add(SExpression next) {
		if (next != null)
			ListValue.add(next);
	}
	
	/**
	 * Convert to a regular literal/list.
	 * @return A literal / list.
	 */
	public SchemeObject<?> deSExpression() {
		if (isLiteral())
			return getLiteral();
		else {
			SchemeObject<?>[] ls = new SchemeObject<?>[getList().size()];
			for (int i = 0; i < ls.length; i++)
				ls[i] = getList().get(i).deSExpression();
			return SchemePair.fromList(ls);
		}
	}
}
