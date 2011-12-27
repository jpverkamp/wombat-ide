package wombat.scheme;

import java.util.*;

/**
 * Parser object, use Parser.parse(...)
 */
public class Parser {
	String Code; // Full code to be parsed
	int Index; // Current index into that code
	int Line, Column; // Line and column number of the index.
	
	/**
	 * Initialize a parsing object.
	 */
	private Parser() {
		
	}

	/**
	 * Parse code into a list of e-expressions. Each will be evaluated in turn.
	 * @param code The code to parse.
	 * @return The list of s-expressions.
	 */
	public static List<SExpression> parse(String code) {
		List<SExpression> result = new ArrayList<SExpression>();
		
		
		
		
		return result;
	}
}
