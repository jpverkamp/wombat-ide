package wombat.scheme.libraries.types;

import java.awt.Color;

/**
 * Store information about a line (used by turtle graphics).
 */
public class LineData {
	public int Tick;
	public double X0;
	public double Y0;
	public double X1;
	public double Y1;
	public Color C;
	
	/**
	 * Create a new line.
	 * @param tick When the line was written.
	 * @param x0 Origin x-coordinate
	 * @param y0 Origin y-coordinate
	 * @param x1 Destination x-coordinate
	 * @param y1 Destination y-coordinate
	 * @param c Line color
	 */
	public LineData(int tick, double x0, double y0, double x1, double y1, Color c) {
		Tick = tick;
		X0 = x0;
		Y0 = y0;
		X1 = x1;
		Y1 = y1;
		C = c;
	}
	
	/**
	 * Stringify the line.
	 */
	public String toString() { 
		return "#[LineData " + X0 + " " + Y0 + " " + X1 + " " + Y1 + " " + C + "]";
	}
}