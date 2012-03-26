package wombat.gui.text;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;

import javax.swing.*;

import wombat.util.Options;

public class LineNumberPanel extends JPanel {
	private static final long serialVersionUID = -1187262329163881259L;
	
	final SchemeTextArea STA;
	int currentValue = 0;
	
	/**
	 * Create a new panel showing line numbers.
	 * @param schemeTextArea The scheme text area this panel is associated with. 
	 * @param scroll The scroll pane containing the text area.
	 */
	public LineNumberPanel(SchemeTextArea schemeTextArea, JScrollPane scroll) {
		STA = schemeTextArea;
		
		final LineNumberPanel me = this;
		scroll.getVerticalScrollBar().addAdjustmentListener(new AdjustmentListener() {
			
			@Override
			public void adjustmentValueChanged(AdjustmentEvent event) {
				currentValue = event.getValue();
				me.repaint();
			}
		});
	}

	@Override
	public void paint(Graphics g) {
		super.paint(g);
			
		int off = Options.FontHeight - currentValue % Options.FontHeight;
		int min = 1 + (currentValue / Options.FontHeight);
		int max = 1 + ((currentValue + STA.code.getHeight()) / Options.FontHeight);
		
		int digits = 1 + (int) Math.log10(max);
		
		Dimension d = new Dimension(Options.FontWidth * digits + 4, STA.getHeight());
		this.setSize(d);
		this.setMinimumSize(d);
		this.setPreferredSize(d);
		
		g.setFont(Options.Font);
		g.setColor(Color.BLACK);
		for (int i = min; i < max; i++) {
			g.drawString("" + i, 0, off + (i - min) * Options.FontHeight);
		}
	}
}
