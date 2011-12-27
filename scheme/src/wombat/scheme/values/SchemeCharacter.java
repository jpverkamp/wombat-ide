package wombat.scheme.values;

import wombat.scheme.errors.SchemeSyntaxError;

/**
 * Single characters.
 */
public class SchemeCharacter extends SchemeObject<Character> {
	private static final long serialVersionUID = -4267764093133635713L;

	static final String[][] SpecialCharacters = new String[][]{
		{" ", "space"}, 
	};
	
	/**
	 * Create a new character.
	 * @param value The new character.
	 */
	public SchemeCharacter(Character value) {
		super(value);
	}
	
	public SchemeCharacter(String value) {
		super(null);
		
		if (value.length() == 1)
			Value = value.charAt(0);
		
		for (String[] pair : SpecialCharacters)
			if (pair[1].equals(value))
				Value = pair[0].charAt(0);
		
		if (Value == null)
			throw new SchemeSyntaxError(this, "Bad character constant '" + value + "'");
	}
	
	/**
	 * Return a human-readable version of the object (does not have to be machine readable).
	 * @return That string.
	 */
	public String display() {
		return Value.toString();
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		return "#\\" + Value;
	}

}
