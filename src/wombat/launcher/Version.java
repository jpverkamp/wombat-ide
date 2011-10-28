package wombat.launcher;

import java.util.*;

/**
 * Represents version information.
 */
class Version implements Comparable<Version> {
	String Name;
	String File;
	String Version;
	List<Integer> parts;

	/**
	 * Create a new version record.
	 * 
	 * @param name
	 *            Name of the thing being updated.
	 * @param version
	 *            The current version.
	 * @param file
	 *            The filename that the version uses.
	 */
	public Version(String name, String version, String file) {
		Name = name;
		File = file;
		Version = version;

		parts = new ArrayList<Integer>();

		for (String s : version.split(".")) {
			try {
				parts.add(Integer.parseInt(s));
			} catch (NumberFormatException e) {
				parts.add((int) s.charAt(0));
			}
		}
	}

	/**
	 * Compare two versions.
	 */
	@Override
	public int compareTo(Version that) {
		for (int i = 0; i < Math.min(parts.size(), that.parts.size()); i++)
			if (parts.get(i) < that.parts.get(i))
				return -1;
			else if (parts.get(i) > that.parts.get(i))
				return 1;

		if (parts.size() < that.parts.size())
			return -1;
		else if (parts.size() > that.parts.size())
			return 1;
		else
			return 0;
	}

	/**
	 * Parse a version string.
	 * 
	 * @param s
	 *            The version string (common delimited, each line is
	 *            Name,Version,File)
	 * @return A map of names to versions (see parameters)
	 */
	public static Map<String, Version> parseVersions(String s) {
		Map<String, Version> result = new HashMap<String, Version>();

		for (String line : s.split("\n")) {
			String[] parts = line.split(",");
			if (parts.length != 3)
				continue;

			result.put(parts[0], new Version(parts[0], parts[1], parts[2]));
		}

		return result;
	}
}