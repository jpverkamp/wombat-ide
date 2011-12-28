package values;

/**
 * Identifiers.
 */
public class SchemeSymbol extends SchemeObject<String> {
	private static final long serialVersionUID = 2473326947081851400L;
	
	/**
	 * Create a new Symbol.
	 * @param value The string version of the name.
	 */
	public SchemeSymbol(String value) {
		super(value);
	}
}
