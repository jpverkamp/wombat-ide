package wombat.scheme;

/**
 * Listen for changes in the Petite state.
 */
public interface PetiteListener {
	
	/**
	 * This method will be called whenever the Petite system is ready to accept input.
	 */
	public void onReady();
	
	/**
	 * This method will be called whenever a call to the Java-interop returns.
	 */
	public void onInteropReturn();
	
	/**
	 * Called when there is something to send from Petite to any listeners.
	 * @param output The output string.
	 */
	public void onOutput(String output);
}
