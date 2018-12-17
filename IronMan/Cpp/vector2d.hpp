#pragma once
#include <vector>

namespace std {
	
	
	
	template <typename T, unsigned nrows, unsigned ncols>
	class vector2d {
	
	private:
		std::vector<T> vec;
		
	public:
	
		vector2d(T arr[][ncols]) {
			vec = std::vector<T> (nrows*ncols, 0);
			clone(arr);
		}
		
		
		
		vector2d(){
			vec = std::vector<T>(nrows * ncols, 0);
		}
		
		void clone(T arr[][ncols]){
			for (int i=0; i<nrows; i++){
				for (int j=0; j<ncols; j++){
					put(i,j,arr[i][j]);
				}
			}
		}
		
		T get(int row, int col) {
			return vec[(row * ncols) + col];
		}
		
		void put(int row, int col, T val) {
			vec[(row * ncols) + col] = val;
		}
		
	};
	
}