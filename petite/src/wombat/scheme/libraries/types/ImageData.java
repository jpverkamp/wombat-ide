/* 
 * License: source-license.txt
 * If this code is used independently, copy the license here.
 */

package wombat.scheme.libraries.types;

/**
 * Store image data.
 */
public class ImageData {
	public int Width;
	public int Height;
	public int[] Data; // for(row) for (col)
	
	/**
	 * Create a new empty image.
	 * @param width Image width.
	 * @param height Image height.
	 */
	public ImageData(int width, int height) {
		Width = width;
		Height = height;
		Data = new int[Width * Height];
	}
	
	/**
	 * Create a new image with content.
	 * @param width Image width.
	 * @param height Image height.
	 * @param data Image data.
	 */
	public ImageData(int width, int height, int[] data) {
		Width = width;
		Height = height;
		Data = data;
	}
}
