package wombat.scheme;

import java.util.*;

import wombat.scheme.errors.SchemeParseError;

/**
 * Parser object, use Parser.parse(...)
 */
public class Parser {
	String Code; // Full code to be parsed
	int Index; // Current index into that code
	int Line, Column; // Line and column number of the index.
	
	/**
	 * Parse code into a list of e-expressions. Each will be evaluated in turn.
	 * @param code The code to parse.
	 * @return The list of s-expressions.
	 */
	public static List<SExpression> parse(String code) {
		Parser p = new Parser(code);
		
		List<SExpression> result = new ArrayList<SExpression>();
		
		SExpression next;
		while (p.hasNext()) {
			next = p.next();
			if (next != null)
				result.add(next);
		}
		return result;
	}
	
	/**
	 * Initialize a parsing object.
	 */
	private Parser(String code) {
		Code = code;
		Index = 0;
		Line = 0;
		Column = 0;
	}
	
	/**
	 * Is there (potentially) another expression to parse?
	 * @return True or false.
	 */
	public boolean hasNext() {
		return Index == Code.length();
		
	}
	
	/**
	 * Parse a single s-Expression.
	 * @return The next s-expression.
	 */
	public SExpression next() {
		// Removing leading whitespace.
		for (; Index < Code.length(); Index++) {
			if (Character.isWhitespace(Code.charAt(Index))) {
				if (Code.charAt(Index) == '\n') {
					Line += 1;
					Column = 0;
				} else {
					Column += 1;
				}
			} else {
				break;
			}
		}
		
		// If we finished, return.
		if (Index >= Code.length())
			return null;
			
		// Check for the opening of a new s-expression.
		if (Code.charAt(Index) == '(' || Code.charAt(Index) == '[') {
			char endBracket = (Code.charAt(Index) == '(' ? ')' : ']');
			
			SExpression sublist = SExpression.list().at(Line, Column);
			
			Index += 1;
			Column += 1;
			
			SExpression subexp;
			while (hasNext()) {
				subexp = next();
				if (subexp == null)
					break;
				else
					sublist.add(subexp);
			}
			
			// Check for the end.
			if (Index < Code.length() && Code.charAt(Index) == endBracket) {
				Index += 1;
				Column += 1;
				return sublist;
			}
			
			// Failed to match parenthesis, signal an error.
			else 
				throw new SchemeParseError(sublist, "Mismatched brackets.");
		}
	
		
		
		
		// Error!
		return null;
	}
}
