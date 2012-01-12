package wombat.launcher;

import java.util.*;

/**
 * Store information about a file version.
 */
public class Version implements Comparable<Version> {
	String Name;
	String Filename;
	String OS;
	String VersionString;
	List<Integer> Version;
	
	/**
	 * Parse a version from a version line.
	 * 
	 * @param line "{name},{version},{filename}" or "{name},{version},{os},{filename}" 
	 * 	name is a short name for the package to report to the user
	 *  filename is the filename used both locally and remotely
	 *  os is null for any os or one of linux/win/osx (more might be added later)
	 *  version is a version string (a sequence of dot delimited numbers) 
	 */
	public Version(String line) {
		String[] parts = line.split(",");
		if (!(parts.length == 3 || parts.length == 4))
			throw new Error("Invalid version format: " + line);
		
		Name = parts[0];
		Filename = parts[parts.length - 1];
		
		if (parts.length == 4) {
			OS = parts[2].toLowerCase();
			if (!("linux".equals(OS)
					|| "osx".equals(OS)
					|| "win".equals(OS)))
				throw new Error("Invalid OS: " + OS);
		}
		
		VersionString = parts[1];
		Version = new ArrayList<Integer>();
		String[] intParts = parts[1].split("\\.");
		for (String intPart : intParts) {
			try {
				Version.add(Integer.parseInt(intPart));
			} catch(Exception e) {
				throw new Error("Invalid version specification: " + parts[1]);
			}
		}
	}

	/**
	 * Compare two versions.
	 */
	@Override
	public int compareTo(Version that) {
		int maxi = Math.min(Version.size(), that.Version.size());
		int diff = 0;
		for (int i = 0; i < maxi; i++) {
			diff = Version.get(i).compareTo(that.Version.get(i));
			if (diff != 0)
				return diff;
		}
			
		return ((Integer) Version.size()).compareTo((Integer) that.Version.size());
	}

	/**
	 * Check if we should match the given OS.
	 * @param os The OS to match.
	 * @return If our OS matches.
	 */
	public boolean forOS(String os) {
		return OS == null || OS.equals(os);
	}
	
	/**
	 * Write it back out as a string.
	 */
	public String toString() {
		// @param line "{name},{version},{filename}" or "{name},{version},{os},{filename}"
		return Name + "," + VersionString + "," + (OS == null ? "" : OS + ",") + Filename; 
	}
}
