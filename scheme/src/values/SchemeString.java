package values;

/**
 * A sequence of characters.
 */
public class SchemeString extends SchemeObject<String> {
	private static final long serialVersionUID = 2089963314074820927L;
	
	static final String[][] nonPrintable = new String[][] {
		{"\t", "\\t"},
		{"\n", "\\n"},
		{"\'", "\\'"},
		{"\"", "\\\""},
	};

	/**
	 * Create a string.
	 * @param value The String.
	 */
	public SchemeString(String value) {
		super(value);
	}
	
	/**
	 * Return a machine-readable version of the object.
	 * @return That string.
	 */
	public String write() {
		String result = Value;
		for (String[] pair : nonPrintable) 
			result.replace(pair[0], pair[1]);
		return result;
	}
}
