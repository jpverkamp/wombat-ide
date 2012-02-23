package wombat.scheme.libraries;

/**
 * Store image data.
 */
public class ImageData {
	public int Width;
	public int Height;
	public int[] Data; // for(row) for (col)
	
	public ImageData(int width, int height) {
		Width = width;
		Height = height;
		Data = new int[Width * Height];
	}
	
	public ImageData(int width, int height, int[] data) {
		Width = width;
		Height = height;
		Data = data;
	}
}
