package wombat.scheme.util;

import wombat.scheme.libraries.*;
import wombat.util.Base64;

/**
 * Helper that stores all of the interop functions.
 */
public class InteropAPI {
	private InteropAPI() {}
	
	/**
	 * Interop method.
	 * @param key The method name.
	 * @param val Any parameters.
	 * @return Either null to send nothing or a string to send back.
	 */
	public static String interop(String key, String val) {
		key = key.toLowerCase();
		
		try {
			
			// Image API
			if ("read-image".equals(key)) {
				ImageData img = null;
				if (val == null)
					img = ImageAPI.readImage();
				else
					img = ImageAPI.readImage(val);
				
				System.out.println("data length: " + img.Data.length);
				
				return img.Width + " " + img.Height + " \"" + Base64.encodeBytes(Conversion.int2byte(img.Data)) + "\"";
			} else if ("write-image".equals(key)) {
				String[] args = val.split(" ");
				ImageData img = new ImageData(
					Integer.parseInt(args[0]),
					Integer.parseInt(args[1]),
					Conversion.byte2int(Base64.decode(args[2]))
				);
				
				if (args.length == 3)
					ImageAPI.writeImage(img);
				else
					ImageAPI.writeImage(img, args[3]);
			}
			
			// Test method.
			else if ("fact".equals(key)) {
				int n = Integer.parseInt(val);
				int a = 1;
				for (int i = 2; i <= n; i++) a *= i;
				return "" + a;
			}
			
		} catch(Exception e) {
			return "Exception in " + key + ": " + e.getMessage();
		}
		
		return null;
	}
}
