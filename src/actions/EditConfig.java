package actions;

import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import gui.*;

import javax.swing.AbstractAction;

/**
 * Edit the config file.
 */
public class EditConfig extends AbstractAction {
	private static final long serialVersionUID = 5083860489087944835L;

	@Override
	public void actionPerformed(ActionEvent arg0) {
		File f = new File(Options.OPTIONS_FILE);
        if (!f.exists())
        {
        	try
            {
                // Get the input stream from the JAR.
                InputStream fromJar;
                if ((fromJar = getClass().getResourceAsStream(Options.OPTIONS_FILE)) == null)
                    if ((fromJar = getClass().getResourceAsStream("/" + Options.OPTIONS_FILE)) == null)
                        throw new FileNotFoundException(Options.OPTIONS_FILE);

                // Get the output stream to the file.
                OutputStream toFile = new FileOutputStream(new File(Options.OPTIONS_FILE));

                // Copy.
                byte[] buf = new byte[8192];
                while (true) {
                  int length = fromJar.read(buf);
                  if (length < 0)
                    break;
                  toFile.write(buf, 0, length);
                }

                // Close buffers.
                fromJar.close();
                toFile.close();
            }
            catch (FileNotFoundException ex)
            {
                ex.printStackTrace();
            }
            catch (IOException ex)
            {
                ex.printStackTrace();
            }
        }
        MainFrame.me().Documents.Open(f);
	}
}