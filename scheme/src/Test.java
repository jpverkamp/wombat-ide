import java.util.Scanner;

import wombat.scheme.WombatScheme;

public class Test {
	public static void main(String[] args) {
		WombatScheme scheme = new WombatScheme();
		Scanner in = new Scanner(System.in);
		
		String line;
		while (scheme.isRunning()) {
			System.out.print("> ");
			System.out.flush();
			line = in.nextLine();
			System.out.println(scheme.run(line));
			if ("(exit)".equals(line))
				break;
		}
	}
}
