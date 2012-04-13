package wombat.scheme.libraries.types;

import java.awt.Color;

/**
 * Store a current turtle's state.
 */
public class TurtleData {
	public double X = 0;
	public double Y = 0;
	public double R = 0;
	public boolean Pen = true;
	public Color C = Color.BLACK;
	
	public boolean Live = true;
	
	/**
	 * Create a new default turtle.
	 */
	public TurtleData() {
	}
	
	/**
	 * Create a new non-default Turtle.
	 * @param x X-location
	 * @param y Y-location
	 * @param r Rotation (in radians)
	 * @param pen If the pen is down.
	 * @param c The pen color.
	 */
	public TurtleData(double x, double y, double r, boolean pen, Color c) {
		X = x;
		Y = y;
		R = r;
		Pen = pen;
		C = c;
	}
	
	/**
	 * String representation.
	 */
	public String toString() {
		return "#[TurtleData " + X + " " + Y + " " + R + " " + Pen + " " + C + "]";
	}
}
