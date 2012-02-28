package wombat.scheme.libraries.gui;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Robot;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.geom.AffineTransform;
import java.text.DecimalFormat;

import javax.swing.*;

/**
 * A semi-advanced image viewer.
 * 
 *  Features:
 *  - zoom in on images / parts of images
 *  - hover over the image to get RGB values
 */
public class ImageFrame extends JFrame implements MouseMotionListener {
	private static final long serialVersionUID = 138774504307103027L;

	Image MyImage;
	Robot MyRobot;
	
	double Scale = 1.0;
	boolean Debug = true;
	
	int ImageX = -1;
	int ImageY = -1;
	int ScreenX = -1;
	int ScreenY = -1;
	
	static final int MINIMUM_DISPLAY_WIDTH = 300;
	static final int MINIMUM_DISPLAY_HEIGHT = 100;

	static final int ROW_HEIGHT = 14;
	static final DecimalFormat FORMAT = new DecimalFormat("#.##");
	
	/**
	 * Create the image panel.
	 * @param image Image data to display.
	 */
	public ImageFrame(Image image) {
		// Basic layout stuff.
		setTitle("draw-image");
		setLayout(new BorderLayout());
		setLocationByPlatform(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		pack();
		
		MyImage = image; 
		try { MyRobot = new Robot(); } catch (AWTException e) {}
		
		// Try to figure out a good size to display the image at.
		Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		
		int width = MyImage.getWidth(null);
		int height = MyImage.getHeight(null);
		
		if (width < MINIMUM_DISPLAY_WIDTH) width = MINIMUM_DISPLAY_WIDTH;
		if (height < MINIMUM_DISPLAY_HEIGHT) height = MINIMUM_DISPLAY_HEIGHT;
		
		if (width + 2 * ROW_HEIGHT > dim.getWidth()) width = (int) dim.getWidth() - 2 * ROW_HEIGHT;
		if (height + 6 * ROW_HEIGHT > dim.getHeight()) height = (int) dim.getHeight() - 6 * ROW_HEIGHT;
		
		setSize(width + 2 * ROW_HEIGHT, height + 6 * ROW_HEIGHT);
		
		add(new ImagePanel());
		
		addMouseMotionListener(this);
	}
	
	/**
	 * Panel so that we can override the paintComponent method.
	 */
	class ImagePanel extends JPanel {
		private static final long serialVersionUID = -2370331213089804268L;

		/**
		 * Custom paint method.
		 */
		public void paintComponent(Graphics graphics) {
			super.paintComponent(graphics);
			Graphics2D g = (Graphics2D) graphics;
	
			// Draw the background.
			g.setColor(Color.WHITE);
			g.fillRect(0,  0, getWidth(), getHeight());
	
			// Calculate the image scale.
			Scale = Math.min(
				(double) (getWidth() - 2 * ROW_HEIGHT) / (double) MyImage.getWidth(null),
				(double) (getHeight() - 6 * ROW_HEIGHT) / (double) MyImage.getHeight(null)
			);
			
			// Get the scaled width and height of the image.
			int scaleWidth = (int) (Scale * MyImage.getWidth(null));
			int scaleHeight = (int) (Scale * MyImage.getHeight(null));
			int left = ((getWidth() - 2 * ROW_HEIGHT) - scaleWidth) / 2 + ROW_HEIGHT;
			int top = ((getHeight() - 6 * ROW_HEIGHT) - scaleHeight) / 2 + ROW_HEIGHT;
			
			// Finally, draw the image.
			g.translate(left, top);
			g.drawImage(MyImage, AffineTransform.getScaleInstance(Scale, Scale), null);
			
			// Draw a border.
	//		g.setColor(Color.BLACK);
	//		g.setStroke(new BasicStroke(2.0f));
	//		g.drawRect(0, 0, scaleWidth, scaleHeight);
			
			// Print image information (if requested)
			if (Debug) {
				g.setColor(Color.BLACK);
				g.translate(-left, scaleHeight);
				
				g.drawString(
					"Image size: " + 
						MyImage.getHeight(null) + " rows, " + 
						MyImage.getWidth(null) + " columns",
					ROW_HEIGHT, 2 * ROW_HEIGHT);
				g.drawString(
					"Current scale: " + FORMAT.format(100 * Scale) + "%", 
					ROW_HEIGHT, 3 * ROW_HEIGHT);
				
				if (MyRobot != null 
						&& ImageX >= left && ImageX <= left + scaleWidth 
						&& ImageY >= top && ImageY <= top + scaleHeight) {
					
					int displayX = (int) ((ImageX - left) / Scale);
					int displayY = (int) ((ImageY - top) / Scale);
					
					Color c = MyRobot.getPixelColor(ScreenX, ScreenY);
					g.drawString(
						"Color at " + 
							"row " + displayY + " / column " + displayX + ": " +
							"#[color " + c.getRed() + " " + c.getGreen() + " " + c.getBlue() + "]", 
							ROW_HEIGHT, 4 * ROW_HEIGHT);
				} else {
					g.drawString(
						"Mouse over the image to view colors", 
						ROW_HEIGHT, 4 * ROW_HEIGHT);
				}
			}
		}
	}

	/**
	 * When the mouse is dragged (drug?)
	 * @param event Details.
	 */
	@Override public void mouseDragged(MouseEvent event) {}

	/**
	 * When the mouse is moved.
	 * @param event Details
	 */
	@Override public void mouseMoved(MouseEvent event) {
		ImageX = event.getX();
		ImageY = event.getY();
		ScreenX = event.getXOnScreen();
		ScreenY = event.getYOnScreen();
		repaint();
	}
}
