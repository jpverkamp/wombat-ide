package wombat.scheme;

import java.util.*;

import wombat.scheme.errors.SchemeRuntimeError;
import wombat.scheme.library.Base;
import wombat.scheme.values.*;


/**
 * Evaluation environment.
 */
public class Environment {
	Environment Parent;
	Map<String, SchemeObject<?>> Values;
	boolean IsBase = false;
	
	/**
	 * No constructor, use empty() or extend().
	 */
	private Environment() {
		Values = new HashMap<String, SchemeObject<?>>();
	}
	
	/**
	 * Create a new empty environment.
	 * @return The empty environment.
	 */
	public static Environment empty() {
		Environment env = new Environment();
		return env;
	}
	
	/**
	 * Load an environment with all of the base parameters loaded.
	 * @return The base environment.
	 */
	public static Environment base() {
		Environment env = new Environment();
		Base.load(env);
		env.IsBase = true;
		return env;
	}
	
	/**
	 * Create an environment building another layer on the current environment.
	 * @return A new environment.
	 */
	public Environment extend() {
		Environment env = new Environment();
		env.Parent = this;
		return env;
	}
	
	/**
	 * Access an object.
	 * @param key A symbol used as the key.
	 * @return It's value.
	 */
	public SchemeObject<?> get(SchemeSymbol key) {
		String skey = key.getValue();
		
		if (Values.containsKey(skey))
			return Values.get(skey);
		else if (Parent != null)
			return Parent.get(key);
		else
			throw new SchemeRuntimeError(key, "Unbound variable '" + key.display() + "'");
	}
	
	/**
	 * Set an already defined variable.
	 * @param key The key to save it to.
	 * @param val The value to store.
	 */
	public void set(SchemeSymbol key, SchemeObject<?> val) {
		String skey = key.getValue();
		
		if (Values.containsKey(skey))
			Values.put(skey, val);
		else if (Parent != null)
			Parent.set(key, val);
		else
			throw new SchemeRuntimeError(key, "Cannot set undefined variable '" + key.display() + "'");
	}

	/**
	 * Define a new variable.
	 * @param key The key to save the variable in (at the current level).
	 * @param val The initial value to store.
	 */
	public void define(SchemeSymbol key, SchemeObject<?> val) {
		Values.put(key.getValue(), val);
	}
	
	/**
	 * Define a new procedure (they already contain their name).
	 * @param proc The new procedure.
	 */
	public void defineProcedure(SchemeProcedure proc) {
		Values.put(proc.getName(), proc);
	}
	
	/**
	 * Define a new macro (they already contain their name).
	 * @param proc The new macro.
	 */
	public void defineMacro(SchemeMacro proc) {
		Values.put(proc.getName(), proc);
	}
	
	/**
	 * Debug print for environments. 
	 * @return The default print.
	 */
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("{");
		
		if (IsBase) {
			sb.append("§");
		} else {
			for (String key : Values.keySet()) {
				sb.append(key);
				sb.append("=");
				sb.append(Values.get(key));
				sb.append(", ");
			}
			if (!Values.isEmpty()) {
				sb.delete(sb.length() - 2, sb.length());
				sb.append("; ");
			}
			if (Parent != null) {
				String subs = Parent.toString();
				sb.append(subs.substring(1, subs.length() - 1));
			}
		}
		
		sb.append("}");
			
		return sb.toString();
	}
}
