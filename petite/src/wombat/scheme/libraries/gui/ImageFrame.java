/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.scheme.libraries.gui;

import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.text.DecimalFormat;

import javax.swing.*;
import javax.swing.border.BevelBorder;

import wombat.scheme.libraries.ImageAPI;

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
	
	static final double SCALE_CONSTANT = 1.3;
	
	double Scale = 1.0;
	boolean Debug = true;
	
	int ImageX = -1;
	int ImageY = -1;
	int ScreenX = -1;
	int ScreenY = -1;
	
	int MaxWidth = -200 + (int) java.awt.Toolkit.getDefaultToolkit().getScreenSize().getWidth();
	int MaxHeight = -200 + (int) java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight();

	JPanel ImageDisplayPanel;
	JLabel ImageDisplay;
	JLabel ImageInformation;
	
	static final int ROW_HEIGHT = 14;
	static final DecimalFormat FORMAT = new DecimalFormat("#.##");
	
	/**
	 * Create the image panel.
	 * @param image Image data to display.
	 */
	public ImageFrame(Image image) {
		final ImageFrame me = this;
		MyImage = image;
		
		// Basic layout stuff.
		setTitle("draw-image");
		setLayout(new BorderLayout(10, 10));
		setLocationByPlatform(true);
		setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
		setResizable(false);
		
		// Add zoom buttons to the top.
		JPanel zoomButtons = new JPanel();
		
		final JButton zoomOut = new JButton("-");
		zoomOut.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				
				if (MyImage.getWidth(null) * Scale / SCALE_CONSTANT < 5 || MyImage.getHeight(null) * Scale / SCALE_CONSTANT < 5)
					return;
				
				Scale /= SCALE_CONSTANT;
				
				int scaleWidth = (int) (MyImage.getWidth(null) * Scale);
				int scaleHeight = (int) (MyImage.getHeight(null) * Scale);
				
				ImageDisplay.setIcon(new ImageIcon(MyImage.getScaledInstance(scaleWidth, scaleHeight, Image.SCALE_SMOOTH)));
				Dimension size = new Dimension(scaleWidth, scaleHeight);
				ImageDisplay.setSize(size);
				ImageDisplay.setMinimumSize(size);
				ImageDisplay.setPreferredSize(size);
				ImageDisplay.setMaximumSize(size);
				
				me.updateInformation();
				
				me.pack();
				
				me.invalidate();
				me.validate();
				
				Point p = zoomOut.getLocationOnScreen();
				MyRobot.mouseMove((int) p.getX() + zoomOut.getWidth() / 2, (int) p.getY() + zoomOut.getHeight() / 2);
			}
		});
		zoomButtons.add(zoomOut);
		
		JButton saveCurrent = new JButton("save");
		saveCurrent.addActionListener(new ActionListener() {			
			@Override public void actionPerformed(ActionEvent arg0) {
				try {
					int scaleWidth = (int) (MyImage.getWidth(null) * Scale);
					int scaleHeight = (int) (MyImage.getHeight(null) * Scale);
					
					Image i = MyImage.getScaledInstance(scaleWidth, scaleHeight, Image.SCALE_SMOOTH);
					BufferedImage bi = new BufferedImage(i.getWidth(null), i.getHeight(null), BufferedImage.TYPE_INT_ARGB);
					Graphics g = bi.getGraphics();
					g.drawImage(i, 0, 0, null);
					ImageAPI.writeImage(bi);
					
				} catch (IOException ex) {
					JOptionPane.showMessageDialog(null, "Unable to save image: " + ex.getMessage(), "Unable to save image", JOptionPane.ERROR_MESSAGE);
				}
			}
		});
		zoomButtons.add(saveCurrent);
		
		final JButton zoomIn = new JButton("+");
		zoomIn.addActionListener(new ActionListener() {
			@Override public void actionPerformed(ActionEvent arg0) {
				
				if (MyImage.getWidth(null) * Scale * SCALE_CONSTANT > MaxWidth || MyImage.getHeight(null) * Scale * SCALE_CONSTANT > MaxHeight)
					return;
				
				Scale *= SCALE_CONSTANT;
				
				int scaleWidth = (int) (MyImage.getWidth(null) * Scale);
				int scaleHeight = (int) (MyImage.getHeight(null) * Scale);
				
				ImageDisplay.setIcon(new ImageIcon(MyImage.getScaledInstance(scaleWidth, scaleHeight, Image.SCALE_SMOOTH)));
				Dimension size = new Dimension(scaleWidth, scaleHeight);
				ImageDisplay.setSize(size);
				ImageDisplay.setMinimumSize(size);
				ImageDisplay.setPreferredSize(size);
				ImageDisplay.setMaximumSize(size);
				
				me.updateInformation();
				
				me.pack();
				
				me.invalidate();
				me.validate();
				
				Point p = zoomIn.getLocationOnScreen();
				MyRobot.mouseMove((int) p.getX() + zoomIn.getWidth() / 2, (int) p.getY() + zoomIn.getHeight() / 2);
			}
		});
		zoomButtons.add(zoomIn);
		
		add(zoomButtons, BorderLayout.NORTH);
		
		// Add a description
		ImageInformation = new JLabel();
		Dimension size = new Dimension(350, 60);
		ImageInformation.setHorizontalAlignment(JLabel.CENTER);
		ImageInformation.setSize(size);
		ImageInformation.setMinimumSize(size);
		ImageInformation.setPreferredSize(size);
		ImageInformation.setMaximumSize(size);
		add(ImageInformation, BorderLayout.SOUTH);
		
		// Add the image.
		ImageDisplay = new JLabel();
		ImageDisplay.setIcon(new ImageIcon(MyImage));
		ImageDisplay.setHorizontalAlignment(JLabel.CENTER);
		ImageDisplay.setIconTextGap(0);
		ImageDisplay.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
		
		ImageDisplayPanel = new JPanel();
		ImageDisplayPanel.add(ImageDisplay);
		add(ImageDisplayPanel, BorderLayout.CENTER);
		
		// Get the size from that to display everything.
		updateInformation();
		pack();
		
		try { MyRobot = new Robot(); } catch (AWTException e) {}
		ImageDisplay.addMouseMotionListener(this);
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
		updateInformation();
	}
	
	/**
	 * Update information about the image.
	 */
	void updateInformation() {
		String text = "<html>";
		
		text += "Image size: " + MyImage.getHeight(null) + " rows, " + MyImage.getWidth(null) + " columns\n";
		text += "<br />";
		text += "Current scale: " + FORMAT.format(100 * Scale) + "%";
		text += "<br />";
		
		if (MyRobot != null) {
			int displayX = (int) (ImageX / Scale);
			int displayY = (int) (ImageY / Scale);
			
			if (displayX >= 0 && displayX < MyImage.getWidth(null) && displayY >= 0 && displayY <= MyImage.getHeight(null)) {
				Color c = MyRobot.getPixelColor(ScreenX, ScreenY);
				text += "Color at " + "row " + displayY + " / column " + displayX + ": " + "#[color " + c.getRed() + " " + c.getGreen() + " " + c.getBlue() + "]";
			}
		}
		
		text += "</html>";
			
		ImageInformation.setText(text);
		
		pack();
	}
}
