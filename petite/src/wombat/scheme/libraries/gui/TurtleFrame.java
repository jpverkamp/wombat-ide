package wombat.scheme.libraries.gui;

import java.util.*;

import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;

import wombat.scheme.libraries.TurtleAPI;
import wombat.scheme.libraries.types.*;

/**
 * An extension of the image frame specifically for turtles.
 */
public class TurtleFrame extends ImageFrame {
	private static final long serialVersionUID = -7570576454913232510L;

	/**
	 * Create a new turtle frame.
	 */
	public TurtleFrame() {
		super(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB));
	}
	
	/**
	 * Update the turtle frame.
	 * @param turtles The 
	 * @param lines
	 */
	public void update(Map<String, TurtleData> turtles, List<LineData> lines) {
		setTitle("draw-turtle (live)");
		
		final int SPACING = 10;
		final int RADIUS = 3;
		
		BufferedImage turtleImage = TurtleAPI.linesToImage(lines, SPACING);
		Graphics2D g = (Graphics2D) turtleImage.getGraphics();
		
		double[] minMax = TurtleAPI.linesToMinMax(lines);
		double xOffset = -(minMax[0] - SPACING) - RADIUS;
		double yOffset = -(minMax[2] - SPACING) - RADIUS;
		
		for (TurtleData t : turtles.values()) {
			if (t.Live) { 
				g.setColor(t.C);
				if (t.Pen)
					g.fillOval((int) (t.X + xOffset), (int) (t.Y + yOffset), RADIUS * 2, RADIUS * 2);
				else
					g.drawOval((int) (t.X + xOffset), (int) (t.Y + yOffset), RADIUS * 2, RADIUS * 2);
			}
		}
		
		MyImage = turtleImage;
		
		int scaleWidth = (int) (MyImage.getWidth(null) * Scale);
		int scaleHeight = (int) (MyImage.getHeight(null) * Scale);
		Dimension d = new Dimension(scaleWidth, scaleHeight);
		
		ImageDisplay.setIcon(new ImageIcon(MyImage.getScaledInstance(scaleWidth, scaleHeight, Image.SCALE_SMOOTH)));
		ImageDisplay.setSize(d);
		ImageDisplay.setMinimumSize(d);
		ImageDisplay.setPreferredSize(d);
		ImageDisplay.setMaximumSize(d);
		
		pack();
		
		invalidate();
		validate();
		
		repaint();
	}
}
