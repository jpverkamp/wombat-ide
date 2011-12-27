package wombat.scheme;

import java.util.*;

import wombat.scheme.values.SchemeObject;

/**
 * Either an object or a list of objects.
 */
public class SExpression {
	SchemeObject<?> LiteralValue;
	List<SExpression> ListValue;
	
	private SExpression () {}

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
		ListValue.add(SExpression.literal(next));
	}
	
	/**
	 * Add another value to this s-expression's list.
	 * @param next The new value as an s-expression.
	 */
	public void add(SExpression next) {
		ListValue.add(next);
	}
}
