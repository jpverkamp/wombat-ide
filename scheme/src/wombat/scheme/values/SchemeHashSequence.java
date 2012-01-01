package wombat.scheme.values;

/**
 * Meta-identifiers.
 */
public class SchemeHashSequence extends SchemeObject<String> {
	private static final long serialVersionUID = -6601219526974146447L;

	/**
	 * Create a new hash-sequence.
	 * @param value The sequence.
	 */
	public SchemeHashSequence(String value) {
		super(value);
	}
}