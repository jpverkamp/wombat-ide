package wombat.scheme.libraries;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;

import java.util.*;
import javax.swing.*;

/**
 * Represent turtle graphics.
 */
public class TurtleAPI {
	/**
	 * Draw a set of turtle lines.
	 * @param data Lines each with "tick x0 y0 x1 y1 r g b"
	 */
	public static void drawTurtle(String data) {
		@SuppressWarnings("unused")
		class LineData {
			int Tick;
			double X0;
			double Y0;
			double X1;
			double Y1;
			Color C;
			
			LineData(int tick, double x0, double y0, double x1, double y1, Color c) {
				Tick = tick;
				X0 = x0;
				Y0 = y0;
				X1 = x1;
				Y1 = y1;
				C = c;
			}
		}
		
		List<LineData> lines = new ArrayList<LineData>();
		
		double minX = 0;
		double maxX = 0;
		double minY = 0; 
		double maxY = 0;
		
		for (String line : data.split("\n")) {
			String[] parts = line.split(" ");
			if (parts.length != 8) continue;
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
		
		for (LineData line : lines) {
			minX = Math.min(line.X0, Math.min(line.X1, minX));
			maxX = Math.max(line.X0, Math.max(line.X1, maxX));
			minY = Math.min(line.Y0, Math.min(line.Y1, minY));
			maxY = Math.max(line.Y0, Math.max(line.Y1, maxY));
		}
		minX -= 1;
		maxX += 1;
		minY -= 1;
		maxY += 1;
		
		Image i = new BufferedImage((int) (maxX - minX), (int) (maxY - minY), BufferedImage.TYPE_INT_RGB);
		Graphics2D g = (Graphics2D) i.getGraphics();
		
		System.out.println("X: " + minX + " " + maxX + ", Y: " + minY + " " + maxY);
		System.out.println("width: " + i.getWidth(null) + ", height: " + i.getHeight(null));
		
		for (LineData line : lines) {
			System.out.println(
					(int) (line.X0 - minX) + " " +
					(int) (line.Y0 - minY) + " " +
					(int) (line.X1 - minX) + " " +
					(int) (line.Y1 - minY)
					);
			
			g.setColor(line.C);
			g.drawLine(
				(int) (line.X0 - minX), 
				(int) (line.Y0 - minY),
				(int) (line.X1 - minX),
				(int) (line.Y1 - minY)
			);
		}
		
		JFrame frame = new JFrame("draw-turtle");
		frame.setLocationByPlatform(true);
		frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		frame.add(new JLabel(new ImageIcon(i)));
		frame.pack();
		frame.setVisible(true);
		
	}
}
