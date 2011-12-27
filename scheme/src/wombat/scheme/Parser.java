package wombat.scheme;

import java.util.*;
import java.util.regex.*;

import wombat.scheme.errors.*;
import wombat.scheme.values.*;
import wombat.scheme.values.numeric.*;

/**
 * Parser object, use Parser.parse(...)
 */
public class Parser {
	String Code; // Full code to be parsed
	int Index; // Current index into that code
	int LastIndex; // Previously used index, used to detect loops
	int Line, Column; // Line and column number of the index.
	
	// List of parseable literals in the order that they will be applied.
	static final ParsePattern[] LiteralPatterns = new ParsePattern[]{
		new ParsePattern("boolean", "#[tfTF]") {
			SchemeObject<?> read() {
				return new SchemeBoolean(LastMatch.group().toLowerCase().charAt(1) == 't');
			}
		},
		
		new ParsePattern("string", "\"([^\\\"]|\\\\|\\\")*\"") {
			SchemeObject<?> read() {
				return new SchemeString(LastMatch.group().substring(1, LastMatch.group().length() - 1));
			}
		},
		
		new ParsePattern("character", "#\\\\[A-Za-z]+") {
			SchemeObject<?> read() {
				return new SchemeCharacter(LastMatch.group().substring(2));
			}
		},
		
		new ParsePattern("complex", "\\d+(\\.\\d*)?[\\+-]\\d+(\\.\\d*)?i") {
			SchemeObject<?> read() {
				return new SchemeComplex(LastMatch.group());
			}
		},
		
		new ParsePattern("real", "\\d+\\.\\d*") {
			SchemeObject<?> read() {
				return new SchemeReal(LastMatch.group());
			}
		},
		
		new ParsePattern("rational", "\\d+/\\d+") {
			SchemeObject<?> read() {
				return new SchemeRational(LastMatch.group());
			}
		},
		
		new ParsePattern("integer", "\\d") {
			SchemeObject<?> read() {
				return new SchemeInteger(LastMatch.group());
			}
		},
		
		new ParsePattern("symbol", "[A-Za-z0-9!$%&*+-./:<=>?@^_~]+") {
			SchemeObject<?> read() { 
				return new SchemeSymbol(LastMatch.group());
			}
		},
	};
	
	// List of prefixes that turn into symbols with a single argument.

	// List of sublist prefixes (things like quote/unquote/etc)
	static final String[][] ParsePrefixes = new String[][]{
		{"#u8", "bytevector"},
		{",@", "unquote-splicing"},
		{"'#", "vector"},
		{"`", "quasiquote"},
		{",", "unquote"},
		{"'", "quote"},
	};
	
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
		
		for (SExpression each : result)
			System.out.println("parsed: " + each.display());
		
		return result;
	}
	
	/**
	 * Initialize a parsing object.
	 */
	private Parser(String code) {
		Code = code;
		Index = 0;
		LastIndex = -1;
		Line = 0;
		Column = 0;
	}
	
	/**
	 * Is there (potentially) another expression to parse?
	 * @return True or false.
	 */
	public boolean hasNext() {
		return Index < Code.length();
	}
	
	/**
	 * Parse a single s-Expression.
	 * @return The next s-expression.
	 */
	public SExpression next() {
		// Break out of infinite loops
		if (Index < Code.length() && Index == LastIndex)
			throw new SchemeParseError(new SchemeVoid().at(Line, Column), "Unable to continue parsing");
		LastIndex = Index;
		
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
		
		System.out.println("At " + Line + ":" + Column + " -- " + Code.substring(Index));
		
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
	
		// Next, try to match against possible prefixes.
		for (String[] pair : ParsePrefixes) {
			if (Code.substring(Index).startsWith(pair[0])) {
				SExpression longForm = SExpression.literal(new SchemeSymbol(pair[1])).at(Line, Column);
				
				Index += pair[0].length();
				Column += pair[0].length();
				
				return SExpression.list(longForm, next());
			}
		}
		
		// Otherwise, match against literal types.		
		for (ParsePattern pattern : LiteralPatterns) {
			if (pattern.match(Code.substring(Index))) {
				SExpression result = SExpression.literal(pattern.read()).at(Line, Column);
				
				System.out.println("Parsed " + pattern.Type + " -- " + result);
				
				Index += pattern.Length;
				Column += pattern.Length;
				return result;
			}
		}
		
		// Nothing matches, fall up a level.
		return null;
	}
}

/**
 * Represent individual literal parse patterns.
 */
abstract class ParsePattern {
	String Type;
	Pattern Regex;
	Matcher LastMatch;
	int Length;
	
	/**
	 * Create a new parse pattern.
	 * @param type The type to parse (used only for debugging).
	 * @param re The regular expression (matches at the beginning of the string).
	 */
	public ParsePattern(String type, String re) {
		Type = type;
		
		if (re.length() > 1 && !(re.charAt(0) == '^'))
			re = '^' + re;
		
		Regex = Pattern.compile(re);
	}
	
	/**
	 * Does the pattern match the given code?
	 * Sets the LastGroup variable.
	 * 
	 * @param code The code to match.
	 * @return If the code matches the pattern.
	 */
	boolean match(String code) {
		LastMatch = Regex.matcher(code);
		boolean result = LastMatch.find();
		
		if (result)
			Length = LastMatch.group().length();
		else
			Length = -1;
		
		return result;
	}
	
	/**
	 * Use the variable set by match to create a Scheme object.
	 * @return The parsed object.
	 */
	abstract SchemeObject<?> read();
}
