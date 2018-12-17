#pragma once




#include "vector2d.hpp"

//#include <core/core.hpp>
//#include <highgui/highgui.hpp>

static const int lut[256] = {
		0, 0, 0, 1, 0, 0, 1, 3, 0, 0, 3, 1, 1, 0, 1, 3, 0, 0, 0, 0, 0, 0,
		0, 0, 2, 0, 2, 0, 3, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 2, 2, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0,
		0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 3, 0, 2, 0, 0, 0, 3, 1,
		0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 1, 3, 0, 0,
		1, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 2, 3, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3,
		0, 1, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0
};

template<unsigned nrows, unsigned ncols>
class Skeletonizer {

private:
	static const int nrows2 = nrows + 2;
	static const int ncols2 = ncols + 2;
	
	int skeleton[nrows][ncols];
	int _skeleton[nrows2][ncols2];
	int _cleaned_skeleton[nrows2][ncols2];
	int _pixels_cleaned[nrows2 * ncols2];


public:
	
	Skeletonizer(void) {
	
	};
	
	
	Skeletonizer(std::vector2d<int, nrows, ncols> image) {
		
		put_input(image);
		
		run();
		
	};
	
	
	Skeletonizer(const int image[]) {
		
		put_input(image);
		
		run();
		
	};
	
	Skeletonizer(const int image[][ncols]) {
		
		
		put_input(image);
		
		run();
		
	}
	
	int run(const int image[][ncols], int outmap[][ncols], int outpts[][2]) {
		
		put_input(image);
		run();
		return get_output(outmap, outpts);
		
		
	}
	
	void run(const int image[][ncols], int out[][ncols]) {
		
		put_input(image);
		run();
		get_output(out);
		
	}
	
	 int run(const int image[][ncols], int out[][2]) {
		
		put_input(image);
		run();
		return get_output(out);
		
	}
	
	void put_input(const int *image) {
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				_skeleton[x + 1][y + 1] = image[(x * ncols) + y] > 0;
				_cleaned_skeleton[x + 1][y + 1] = _skeleton[x + 1][y + 1];
			}
		}
	}
	
	void put_input(std::vector2d<int, nrows, ncols> image) {
		int v;
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				v = image.get(x, y);
				_skeleton[x + 1][y + 1] = v > 0;
				_cleaned_skeleton[x + 1][y + 1] = _skeleton[x + 1][y + 1];
			}
		}
	}
	
	void put_input(const int image[][ncols]) {
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				_skeleton[x + 1][y + 1] = image[x][y] > 0;
				_cleaned_skeleton[x + 1][y + 1] = _skeleton[x + 1][y + 1];
			}
		}
	}
	
	void run() {
		
		
		int neighbors;
		bool firstPass;
		bool pixelRemoved = true;
		bool doRmPixel;
		
		while (pixelRemoved) {
			pixelRemoved = false;
			for (int passNum = 0, k = 0, l = 0; passNum < 2; passNum++) {
				firstPass = passNum == 0;
				for (int x = 1; x < nrows2 - 1; x++) {
					for (int y = 1; y < ncols2 - 1; y++) {
						if (_skeleton[x][y] == 0) continue;
						
						neighbors = lut[
								1 * _skeleton[x - 1][y - 1] + 2 * _skeleton[x - 1][y + 0] +
								4 * _skeleton[x - 1][y + 1] + 8 * _skeleton[x + 0][y + 1] +
								16 * _skeleton[x + 1][y + 1] + 32 * _skeleton[x + 1][y + 0] +
								64 * _skeleton[x + 1][y - 1] + 128 * _skeleton[x + 0][y - 1]
						];
						
						doRmPixel = (neighbors == 3) ||
						            (neighbors == 1 and firstPass) ||
						            (neighbors == 2 and not firstPass);
						
						if (doRmPixel) {
							l = (x * ncols2) + y;
							_pixels_cleaned[k++] = l;
							_cleaned_skeleton[x][y] = 0;
							pixelRemoved = true;
						}
					}
					
				}
				
				int x, y;
				for (int i = 0; i < k; i++) {
					l = _pixels_cleaned[i];
					y = l % nrows2;
					x = l / ncols2;
					_skeleton[x][y] = _cleaned_skeleton[x][y];
					
				}
			}
		}
		
		for (int x = 1; x < nrows2 - 1; x++) {
			for (int y = 1; y < ncols2 - 1; y++) {
				skeleton[x - 1][y - 1] = _skeleton[x][y];
			}
		}
		
		
	};
	
	
	std::vector2d<int, nrows, ncols> get_output() {
		return std::vector2d<int, nrows, ncols>(skeleton);
	};
	
	void get_output(std::vector2d<int, nrows, ncols> &out) {
		out.clone(skeleton);
	}
	
	int get_output(int out[][ncols], int outpts[][2]) {
		int k = 0;
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				out[x][y] = skeleton[x][y];
				if (skeleton[x][y]) {
					outpts[k][0] = x;
					outpts[k][1] = y;
					k++;
				}
			}
		}
		return k;
	}
	
	void get_output(int out[][ncols]) {
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				out[x][y] = skeleton[x ][y];
			}
		}
	}
	
	int get_output(int out[][2]) {
		int k = 0;
		for (int x = 0; x < nrows; x++) {
			for (int y = 0; y < ncols; y++) {
				if (skeleton[x][y]) {
					out[k][0] = x;
					out[k][1] = y;
					k++;
				}
			}
		}
		return k;
	}
	
	
};


