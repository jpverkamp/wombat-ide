package wombat.util;

import java.util.Random;

public class NameGenerator {
	private static final char[] consos = "bcdfghjklmnprstvwxyz".toCharArray();
	private static final char[] vowels = "aeiou".toCharArray();
	private static final Random r = new Random();
	
	private NameGenerator() {}
	
	public static String getName() {
		StringBuilder name = new StringBuilder();
		for (int i = 0; i < 4; i++) {
			name.append(consos[r.nextInt(consos.length)]);
			name.append(vowels[r.nextInt(vowels.length)]);
		}
		return name.toString();
	}
}
