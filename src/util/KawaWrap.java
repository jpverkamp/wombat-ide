package util;

import java.util.Stack;

import globals.Globals;
import globals.Imageitude;
import globals.Mathiness;
import globals.Randomness;
import globals.Treeitude;
import gui.ErrorFrame;
import kawa.standard.Scheme;

/**
 * Wrapper around Kawa.
 */
public class KawaWrap {
	Scheme kawa;
	
	/**
	 * Connect to Kawa.
	 */
	public KawaWrap()
	{
		// Create the interpreter.
		kawa = new Scheme();
		
		// Load globals.
        for (Globals g : new Globals[]{
        		new Randomness(),
        		new Mathiness(),
        		new Treeitude(),
        		new Imageitude(),
        }) {
        	try {
        		g.addMethods(kawa);
        	} catch(Throwable ex) {
        		ErrorFrame.log("Unable to load globals from " + g.getClass().getName() + ": " + ex.getMessage());
        	}
        }
	}
	
	/**
	 * Evaluate a command, given as a string.
	 * @param s The string to evaluate.
	 * @return The result.
	 */
	public Object eval(String cmd)
	{
		try {
			// TODO: This sucks, but it does make it work.
			StringBuilder sb = new StringBuilder();
			Stack<Character> brackets = new Stack<Character>();
			Object result = null;
			for (char c : cmd.toCharArray())
			{
				// As soon as we have a matched set, evaluate.
				if (brackets.isEmpty() && sb.length() > 0 && "([".indexOf(c) != -1)
				{
					System.out.println("Subcommand is: " + ("(eval '" + sb.toString() + ")"));
					result = kawa.eval("(eval '" + sb.toString() + ")");
					sb = new StringBuilder();
				}
				
				// Append the character (replace square brackets).
				if (c == '[') sb.append('(');
				else if (c == ']') sb.append(')');
				else sb.append(c);
				
				// Match parenthesis.
				if (c == '(') brackets.push(')');
				else if (c == '[') brackets.push(']');
				else if (c == ')' || c == ']') {
					if (!brackets.isEmpty() && brackets.peek() == c)
						brackets.pop();
					else
						break;
				}
			}
			
			// Match whatever is left.
			if (brackets.isEmpty() && sb.length() > 0)
			{
			
				System.out.println("Subcommand is: " + ("(eval '" + sb.toString() + ")"));
				result = kawa.eval("(eval '" + sb.toString() + ")");
			}
			
			System.out.println("Result is " + result);

			// Return the final result.
			if (result == null || result.toString().length() == 0)
				return null;
			else 
				return result;
			
		}  catch(NoSuchFieldError ex) {
			ex.printStackTrace();
			
			return "Unbound variable: " + ex.getMessage();
		} catch (Throwable ex) {
			return "Error: " + ex;
		}
	}
}
