package wombat.scheme.libraries;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;

import java.util.*;

import wombat.scheme.libraries.gui.*;
import wombat.scheme.libraries.types.*;

/**
 * Represent turtle graphics.
 */
public class TurtleAPI {
	static Map<String, TurtleData> LiveTurtles = new HashMap<String, TurtleData>();
	static List<LineData> LiveLines = new ArrayList<LineData>();
	static TurtleFrame tframe;
	public static double Pause = 0.1;
	
	/**
	 * Convert a turtle to an image.
	 * @param data Lines each with "tick x0 y0 x1 y1 r g b" 
	 * @return An image.
	 */
	public static Image turtleToImage(String data) {
		List<LineData> lines = new ArrayList<LineData>();
		
		for (String line : data.split("\n")) {
			String[] parts = line.trim().split(" ");
			if (parts.length == 0) continue;
			if (parts.length != 8) throw new IllegalArgumentException("Malformed turtle data with line: " + line);
			lines.add(new LineData(
				Integer.parseInt(parts[0]),
				Double.parseDouble(parts[1]),
				-1.0 * Double.parseDouble(parts[2]),
				Double.parseDouble(parts[3]),
				-1.0 * Double.parseDouble(parts[4]),
				new Color(
					Integer.parseInt(parts[5]),
					Integer.parseInt(parts[6]),
					Integer.parseInt(parts[7])
					)
				));
		}
		
		return linesToImage(lines, 10);
	}
	
	/**
	 * Find the minimum and maximum x and y.
	 * @param lines The lines to analyze.
	 * @return Minimum and maximum.
	 */
	public static double[] linesToMinMax(List<LineData> lines) {
		double minX = 0;
		double maxX = 0;
		double minY = 0; 
		double maxY = 0;
		
		for (LineData line : lines) {
			minX = Math.min(line.X0, Math.min(line.X1, minX));
			maxX = Math.max(line.X0, Math.max(line.X1, maxX));
			minY = Math.min(line.Y0, Math.min(line.Y1, minY));
			maxY = Math.max(line.Y0, Math.max(line.Y1, maxY));
		}
		
		return new double[]{minX, maxX, minY, maxY};
	}
		
	/**
	 * Convert a set of lines to an image.
	 * @param lines The lines.
	 * @return An image.
	 */
	public static BufferedImage linesToImage(List<LineData> lines, int spacing) {
		double[] minMax = linesToMinMax(lines);
		double minX = minMax[0] - spacing;
		double maxX = minMax[1] + spacing;
		double minY = minMax[2] - spacing;
		double maxY = minMax[3] + spacing;
		
		BufferedImage bi = new BufferedImage((int) (maxX - minX), (int) (maxY - minY), BufferedImage.TYPE_INT_RGB);
		Graphics2D g = (Graphics2D) bi.getGraphics();
		g.setColor(Color.WHITE);
		g.fillRect(0, 0, bi.getWidth(), bi.getHeight());
		
		for (LineData line : lines) {
			g.setColor(line.C);
			g.drawLine(
				(int) (line.X0 - minX), 
				(int) (line.Y0 - minY),
				(int) (line.X1 - minX),
				(int) (line.Y1 - minY)
			);
		}
		
		return bi;
	}
	
	/**
	 * Draw a set of turtle lines.
	 * @param data Lines each with "tick x0 y0 x1 y1 r g b"
	 */
	public static void drawTurtle(String data) {
		ImageFrame iframe = new ImageFrame(turtleToImage(data));
		iframe.setTitle("draw-turtle");
		iframe.setVisible(true);
	}

	/**
	 * Update a live turtle.
	 * @param val The first part is the turtle id, the rest are parameters.
	 */
	public static void updateTurtle(String id, String fun, String[] args) {
//		// <DEBUG>
//		System.out.println("live-turtle: " + id + "::" + fun + Arrays.toString(args));
//		// </DEBUG>
		
		// Initialize turtle frame if necessary.
		if (tframe == null) 
			tframe = new TurtleFrame();
		
		// Reset the display when it's closed.
		if (!tframe.isVisible()) {
			for (TurtleData t : LiveTurtles.values())
				t.Live = false;
			LiveLines.clear();
		}
		
		// Revisiblify the frame.
		tframe.setVisible(true);
		
		// Spawn is a special case
		if ("spawn".equals(fun)) {
			LiveTurtles.put(id, new TurtleData(
				Double.parseDouble(args[0]),
				-1.0 * Double.parseDouble(args[1]),
				Double.parseDouble(args[2]),
				"down".equals(args[3]),
				new Color(
					Integer.parseInt(args[4]),
					Integer.parseInt(args[5]),
					Integer.parseInt(args[6])
				)));
		}
			
        // Otherwise, verify that we've already seen that turtle.
		else if (!LiveTurtles.containsKey(id)) {
			throw new IllegalArgumentException("Unable to resume live turtle state (missing ID). Live turtles must be on before the turtle is spawned.");
		}
		
		// Finally, process any other sort of command.
		
		// Move one of the turtles
		else if ("move".equals(fun)) {
			double new_x = Double.parseDouble(args[0]);
			double new_y = -1.0 * Double.parseDouble(args[1]);
			
			TurtleData t = LiveTurtles.get(id);
			
			if (t.Pen) LiveLines.add(new LineData(0, t.X, t.Y, new_x, new_y, t.C));
			t.X = new_x;
			t.Y = new_y;
			t.Live = true;
		}
		
		// Turn one of the turtles
		else if ("turn".equals(fun)) {
			LiveTurtles.get(id).R = Double.parseDouble(args[0]);
			LiveTurtles.get(id).Live = true;
		}
		
		// Update the pen state (up/down)
		else if ("pen".equals(fun)) {
			LiveTurtles.get(id).Pen = "down".equals(args[0]);
			LiveTurtles.get(id).Live = true;
		}
		
		// Update the pen color
		else if ("pen-color".equals(fun)) {
			LiveTurtles.get(id).C = new Color(
				Integer.parseInt(args[0]),
				Integer.parseInt(args[1]),
				Integer.parseInt(args[2])
			);
			LiveTurtles.get(id).Live = true;
		}
		
		// Reset after a block
		else if ("block-reset".equals(fun)) {
			TurtleData t = LiveTurtles.get(id);
			
			t.X = Double.parseDouble(args[0]);
			t.Y = -1.0 * Double.parseDouble(args[1]);
			t.R = Double.parseDouble(args[2]);
			t.Pen = "down".equals(args[3]);
			t.C = new Color(
				Integer.parseInt(args[4]),
				Integer.parseInt(args[5]),
				Integer.parseInt(args[6])
			);
			t.Live = true;
		}
		
		// Move one of the turtles
		else {
			throw new IllegalArgumentException("Unable to record turtle state, unknown function: " + fun);
		}
		
		// Update the display.
		tframe.update(LiveTurtles, LiveLines);
		try { Thread.sleep((int) (1000 * Pause)); } catch(InterruptedException ex) {}
	}
}
