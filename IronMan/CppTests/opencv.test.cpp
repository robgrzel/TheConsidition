#include <core/core.hpp>
#include <highgui/highgui.hpp>


#include <cv.h>
#include <highgui.h>

typedef uchar cv_Pixel;
typedef cv::Point3_<uint8_t> cv_Pixel1;

void complicated_threshold(cv_Pixel1 &pixel) {
	if (pow(double(pixel.x) / 10, 2.5) > 100) {
		pixel.x = 255;
		pixel.y = 255;
		pixel.z = 255;
	} else {
		pixel.x = 0;
		pixel.y = 0;
		pixel.z = 0;
	}
}

struct Operator {
	void operator()(cv_Pixel1 &pixel, const int *position) const {
		complicated_threshold(pixel);
		
	}
};


int main(int argc, char **argv) {
	float scaleFactor = 0.5f;
	cv::Mat original = cv::imread("skeleton.png");
	
	std::cout << original.flags << std::endl;
	
	cv::Mat scaled;
	cv::resize(original, scaled, cv::Size(0, 0), scaleFactor, scaleFactor, cv::INTER_LANCZOS4);
	for (int i = 0; i < scaled.rows; i++)
		for (int j = 0; j < scaled.cols; j++) {
			// You can now access the pixel value with cv::Vec3b
			cv_Pixel pixel = scaled.at<cv_Pixel>(i,j);
			// Apply complicatedTreshold
			//complicated_threshold(pixel);
			// Put result back
			//scaled.at<cv_Pixel>(i,j) = pixel;
			std::cout << pixel << std::endl;
		}
	
	//  scaled.forEach<cv_Pixel>(Operator());
	
	//	cv::threshold(scaled, scaled, 100,255,cv::THRESH_BINARY );
	
	cv::imwrite("skeleton1.jpg", scaled);
	
	return 0;
}