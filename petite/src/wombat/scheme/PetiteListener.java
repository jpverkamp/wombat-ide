package wombat.scheme;

/**
 * Listen for changes in the Petite state.
 */
public interface PetiteListener {
	/**
	 * This method will be called whenever the Petite system is ready to accept
	 * input.
	 */
	public void onReady();

	/**
	 * Called when there is something to send from Petite to any listeners.
	 * 
	 * @param output
	 *            The output string.
	 */
	public void onOutput(String output);

	/**
	 * Called when the Scheme system is dying horribly.
	 * 
	 * @param ex
	 *            The exception that caused all of the trouble.
	 */
	public void onError(Exception ex);

	/**
	 * Called when the Petite process has been successfully stopped.
	 */
	public void onStop();

	/**
	 * Called when Petite's environment is reset.
	 */
	public void onReset();
}
