package wombat.scheme.libraries;

public class ImageData {
	public int Rows;
	public int Columns;
	public byte[] Data;
	
	public ImageData(int width, int height) {
		Rows = height;
		Columns = width;
		Data = new byte[3 * Rows * Columns];
	}
	
	public ImageData(int width, int height, byte[] data) {
		Rows = height;
		Columns = width;
		Data = data;
	}
}
