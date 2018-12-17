#pragma once

#ifndef OPENCVTEST_MAPDRAW_H
#define OPENCVTEST_MAPDRAW_H

#include "vector2d.hpp"
#include "skeletonize.hpp"
#include "grafs.hpp"


#include <fstream>
#include <thread>
#include <chrono>
#include <curses.h>
#include <ncurses.h>
#define PI 3.14

#define elif else if
#define fori(s, e) for(int i=s; i<e; i++)
#define forj(s, e) for(int j=s; j<e; j++)

#define RGB255(x) short(x/0.255)


typedef struct line2d {
	p2D_t p1;
	p2D_t p2;
	int d;
} line2d_t;


int min(int a, int b) {
	if (a < b) return a;
	else return b;
}

int max(int a, int b) {
	if (a > b) return a;
	else return b;
}

int dist(int x1, int x2, int y1, int y2) {
	return (int) sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
}

int dist(const p2D_t &p1, const p2D_t &p2) {
	return (int) sqrt(pow(p1.x - p2.x, 2) + pow(p1.y - p2.y, 2));
}

int lerp(int v, int u, double i) {
	return int(v + i * (u - v));
}

p2D_t lerp(const p2D_t &p1, const p2D_t &p2, double i) {
	return {lerp(p1.x, p2.x, i), lerp(p1.y, p2.y, i)};
}

int rotate_x(const int xo, int x, int angle) {
	return int((x - xo) * cos(angle * PI / 180.));
}

int rotate_y(const int yo, int y, int angle) {
	return int((y - yo) * sin(angle * PI / 180.));
}

void rotate_point(const p2D_t &po, p2D_t &p, int angle) {
	p.x = int((p.x - po.x) * cos(angle * PI / 180.));
	p.y = int((p.y - po.y) * sin(angle * PI / 180.));
}

int line_from_2pts(int x0, int x1, int y0, int y1, int arr[][2]) {
	
	int d = dist(x0, x1, y0, y1);
	
	fori(0, d + 1) {
		arr[i][0] = lerp(x0, x1, 1. / d * i);
		arr[i][1] = lerp(y0, y1, 1. / d * i);
	}
	
	return d;
	
}


int line_from_2pts(p2D_t &p0, p2D_t &p1, int arr[][2]) {
	
	int d = dist(p0, p1);
	
	p2D_t pm;
	
	fori(0, d + 1) {
		pm = lerp(p0, p1, 1. / d * i);
		arr[i][0] = pm.x;
		arr[i][1] = pm.y;
	}
	
	return d;
	
}

inline int sleep(int millis){
	std::this_thread::sleep_for(std::chrono::milliseconds(millis));
}


namespace map {
	
	const int ncols = 100;
	const int nrows = 100;
	const int ncels = 10000;
	#define NCOLS ncols
	#define NROWS nrows
	#define NCELS ncels
	
	#define COLOR_BROWN 8
	#define COLOR_ORANGE 9
	#define COLOR_GREY 10
	
	int dyndata[NROWS][NCOLS];
	int mapdata[NROWS][NCOLS];
	int waydata[NROWS][NCOLS];
	int skeleton[NROWS][NCOLS];
	int midpoints[NCELS][2];
	
	int nMidpoints;
	
	p2D_t pstart;
	p2D_t pend;
	
	
	typedef enum {
		nowaypoint_e = 0,
		waypoint_e = 1,
		midpoint_e = 2,
		pathpoint_e = 3,
		null_point_e = 10,
	} point_types_e;
	
	
	typedef enum {
		road_e = 1000,
		trail_e = 1100,
		grass_e = 1200,
		water_e = 1300,
		rockywater_e = 1400,
		forest_e = 1500,
		start_e = 1600,
		win_e = 1700,
		null_terrain_e = 0,
	} terrain_types_e;
	
	
	typedef enum {
		grass_nc = 1,
		trail_nc = 2,
		road_nc = 3,
		water_nc = 4,
		rockywater_nc = 5,
		forest_nc = 6,
		start_nc = 7,
		win_nc = 8,
		corner_nc = 9,
		midpoint_nc = 10,
		empty_nc = 11,
		player_nc = 12,
		beam_nc = 13,
		pathpoint_nc = 14,
		null_terrain_nc = 0,
	} terrain_types_nc_e;
	
	typedef enum {
		WIN_CH = 'W',
		START_CH = 'S',
		GRASS_CH = ' ',
		FOREST_CH = '&',
		WATER_CH = '~',
		TRAIL_CH = '-',
		ROAD_CH = '=',
		ROCKYWATER_CH = '^',
		EMPTY_CH = '.',
		PLAYER_CH = '*',
		MIDPOINT_CH = '+',
		BEAM_CH = '*',
		PATHPOINT_CH = ':',
		
	} terrain_signs_nc;
	
	typedef std::unordered_map<int, char> hashmap_terrain_nc_t;
	
	typedef std::pair<terrain_types_e, terrain_types_nc_e> pair_terrain_types_e;
	
	#define unpair_terrain_types_e map::terrain_types_e, map::terrain_types_nc_e
	
	
	static const std::vector<point_types_e> &point_types = {
			pathpoint_e, midpoint_e, waypoint_e, nowaypoint_e,
	};
	
	static const std::vector<terrain_types_e> &cango_terrain_type_e = {
			grass_e, trail_e, road_e, water_e
	};
	
	static const std::vector<terrain_signs_nc> &cango_terrain_signs_nc = {
			WIN_CH, START_CH, GRASS_CH, WATER_CH,
			TRAIL_CH, ROAD_CH, EMPTY_CH, PLAYER_CH,
			MIDPOINT_CH, BEAM_CH, PATHPOINT_CH
	};
	
	
	static const std::vector<pair_terrain_types_e> terrain_types = {
			{win_e,        win_nc},
			{start_e,      start_nc},
			{forest_e,     forest_nc},
			{rockywater_e, rockywater_nc},
			{water_e,      water_nc},
			{grass_e,      grass_nc},
			{trail_e,      trail_nc},
			{road_e,       road_nc},
	};
	
	
	static hashmap_terrain_nc_t signs_nc = {
			{win_nc,        WIN_CH},
			{start_nc,      START_CH},
			{grass_nc,      GRASS_CH},
			{forest_nc,     FOREST_CH},
			{water_nc,      WATER_CH},
			{trail_nc,      TRAIL_CH},
			{road_nc,       ROAD_CH},
			{rockywater_nc, ROCKYWATER_CH},
			{empty_nc,      EMPTY_CH},
			{player_nc,     PLAYER_CH},
			{midpoint_nc,   MIDPOINT_CH},
			{beam_nc,       BEAM_CH},
			{pathpoint_nc,  PATHPOINT_CH}
		
	};
	
	typedef struct Point2d {
		p2D_t p;
		point_types_e pointType = nowaypoint_e;
		terrain_types_e terrainType = null_terrain_e;
		terrain_types_nc_e terrainTypeNc = null_terrain_nc;
		int c = 0;
		int cr = INT32_MAX;
		
	} Point2d_t;
	
	typedef struct Line2d {
		line2d_t l;
		point_types_e pointType = nowaypoint_e;
		terrain_types_e terrainType = null_terrain_e;
		terrain_types_nc_e terrainTypeNc = null_terrain_nc;
		int c = 0;
		int cr = INT32_MAX;
		
	} Line2d_t;
	
	void read_map_dynamic(std::string fdir) {
		std::string path = fdir + "/datadir/";
		std::string dyndataFname = "mapdata.dynamic.csv";
		std::ifstream dyndataInfile(path + dyndataFname);
		
		
		int c, l, x, y;
		
		l = 0;
		while (dyndataInfile >> c) {
			
			y = 100 - (l / 100); //col
			x = l % 100; //row
			
			dyndata[x][y] = c;
			
			/*
			if (dist(x, xP, y, yP) >= d) continue;
			if (mapPart == 1) y -= (LINES - 1);
			if (y >= 0 && y < (LINES + 1))
			draw_dyn(c, x, y);
			*/
			
			l++;
		}
		
	}
	
	void get_start_end_points() {
	
	}
	
	int in_border(int x, int y) {
		return (0 <= x && x < NROWS) && (0 <= y && y < NCOLS);
	}
	
	
	void read_map_static(std::string fdir) {
		std::string path = fdir + "/datadir/";
		std::string mapdataFname = "mapdata.static.csv";
		std::ifstream mapdataInfile(path + mapdataFname);
		
		
		int c, l, x, y;
		
		l = 0;
		
		while (mapdataInfile >> c) {
			
			y = 100 - (l / 100); //col
			x = l % 100; //row
			
			mapdata[x][y] = c;
			
			l++;
		}
		
		
	}
	
	static int cnt = 0;
	
	
	int filter_noways(const p2D_t &p) {
		
		return map::waydata[p.x][p.y] == nowaypoint_e;
		
		
	}
	
	
	int filter_waypoints(int c) {
		
		for (auto tt : map::cango_terrain_type_e) {
			if (c == tt) return true;
		}
		
		return false;
		
	}
	
	void get_start_and_end() {
		
		int v;
		
		bool startFound = false;
		bool endFound = false;
		
		fori(0, NROWS) {
			forj(0, NCOLS) {
				v = mapdata[i][j];
				if (start_e == v) {
					pstart.x = i;
					pstart.y = j;
					startFound = true;
				} else if (win_e == v) {
					pend.x = i;
					pend.y = j;
					endFound = true;
				}
				
				if (startFound && endFound) return;
			}
		}
		
		
	}
	
	Point2d_t make_pathpoint(int x, int y) {
		
		map::Point2d_t pixel;
		
		pixel.p = {x, y};
		
		pixel.pointType = map::pathpoint_e;
		pixel.terrainType = map::null_terrain_e;
		pixel.c = map::pathpoint_e;
		
		
		pixel.terrainTypeNc = map::pathpoint_nc;
		return pixel;
	}
	
	void get_waypoints() {
		fori(0, NROWS) {
			forj(0, NCOLS) {
				waydata[i][j] = filter_waypoints(mapdata[i][j]);
			}
		}
		
		waydata[pstart.x][pstart.y] = 1;
		waydata[pend.x][pend.y] = 1;
	}
	
	Skeletonizer<map::NROWS, map::NCOLS> skeletonizer = {};
	
	int get_midpath() {
		
		nMidpoints = skeletonizer.run(map::waydata, map::skeleton, map::midpoints);
		return nMidpoints;
		
		
	}
	
	int startend_to_file(std::string fdir) {
		FILE *fp;
		
		/* open the file for writing*/
		fp = fopen((fdir + "/datadir/" + "startend.csv").data(), "w");
		
		/* write 10 lines of text into the file stream*/
		fprintf(fp, "%d,%d\n", map::pstart.x, map::pstart.y);
		fprintf(fp, "%d,%d\n", map::pend.x, map::pend.y);
		
		/* close the file*/
		fclose(fp);
		return 0;
	}
	
	
	int waydata_to_file(std::string fdir) {
		FILE *fp;
		
		/* open the file for writing*/
		fp = fopen((fdir + "/datadir/" + "waydata.csv").data(), "w");
		
		/* write 10 lines of text into the file stream*/
		for (auto &i : map::waydata) {
			for (int j : i) {
				fprintf(fp, "%d\n", j);
			}
		}
		
		/* close the file*/
		fclose(fp);
		return 0;
	}
	
	int skeleton_to_file(std::string fdir) {
		FILE *fp;
		
		/* open the file for writing*/
		fp = fopen((fdir + "/datadir/" + "skeleton.csv").data(), "w");
		
		/* write 10 lines of text into the file stream*/
		for (auto &i : map::skeleton) {
			for (int j : i) {
				fprintf(fp, "%d\n", j);
			}
		}
		
		/* close the file*/
		fclose(fp);
		return 0;
	}
	
	int midpoints_to_file(std::string fdir) {
		FILE *fp;
		
		/* open the file for writing*/
		fp = fopen((fdir + "/datadir/" + "midpoints.csv").data(), "w");
		
		/* write 10 lines of text into the file stream*/
		for (int i = 0; i < map::nMidpoints; i++) {
			fprintf(fp, "%d,%d\n", map::midpoints[i][0], map::midpoints[i][1]);
		}
		
		
		/* close the file*/
		fclose(fp);
		return 0;
	}
	
	
	template<typename L, typename R>
	auto get_item_type(int c, const std::vector<std::pair<L, R>> &arr) {
		for (auto t : arr) {
			if (c >= t.first) return t;
		}
	}
	
	template<typename T>
	auto get_item_type(int c, const std::vector<T> &arr) {
		for (auto t : arr) {
			if (c >= t) return t;
		}
	}
	
	void parse_pixel(map::Point2d_t *point, int c) {
		int cr = c;
		
		auto tt = get_item_type<unpair_terrain_types_e>(c, map::terrain_types);
		
		cr -= tt.first;
		
		auto pt = get_item_type<map::point_types_e>(cr, map::point_types);
		
		cr -= pt;
		
		point->c = c;
		point->cr = cr;
		
		point->pointType = pt;
		point->terrainType = tt.first;
		point->terrainTypeNc = tt.second;
		
	}
}

class Map_Drawer {

public:
	
	p2D_t pstart;
	p2D_t pgoal;
	p2D_t pdraw;
	p2D_t pmin;
	p2D_t pmax;
	
	p2D_t p;
	
	p2D_t pprev;
	p2D_t pnext;
	
	int linesPrev;
	
	int maxview = 10;
	
	std::string fdir = ".";
	char movedir = '0';
	
	
	
    gr::Graf_Traversal<
			map::NROWS,
			map::NCOLS,
			gr::NN4_e,
			gr::Square_Grid,
			p2D_t> Graf;

	inline int incx() {
		return ++p.x;
	}
	
	inline int dcrx() {
		return --p.x;
	}
	
	inline int incy() {
		return ++p.y;
	}
	
	inline int dcry() {
		return --p.y;
	}
	
	inline int swap_start_goal(){
		if (pgoal == map::pend){
			movedir = 'b';
			p = map::pend;
			pgoal = map::pstart;
		} else {
			movedir = 't';
			p = map::pstart;
			pgoal = map::pend;
		}
		
		return 0;
	}
	
	int refresh_path(bool doShortest){
		Graf.bfs_from_to(p,pgoal,map::filter_noways, doShortest);
		
		if (1 < Graf.pathto.size()) {
			movedir = 't';
			auto pi = Graf.pathto[1];
			pnext = pi;
			movexy(pi.x, pi.y);
			
			return 0;
		}
		
		return 1;
		
	}
	
	int run_simulation();
	
	int find_terrain_path(p2D_t start, p2D_t end);
	
	int read_map_terrain(bool doSaveCSV);
	
	void update_pdraw();
	
	int in_border(int x, int y);
	
	int moveup();
	
	int movedn();
	
	int movelt();
	
	int movert();
	
	int move_pstart(p2D_t p);
	
	int movep(p2D_t p);
	
	int movexy(int x, int y);
	
	int keys_handler(int ch);
	
	int win_resize_handler();
	
	int show_info();
	
	void update_lims() {
		pmin.x = 0;//max(0,p.x - 30);
		pmin.y = max(0, p.y - LINES / 2);
		pmax.x = map::NROWS;//min(map::NROWS,p.x + 30) + max(0, 30 - pmin.x);
		pmax.y = min(map::NCOLS, p.y + LINES / 2) + max(0, LINES / 2 - pmin.y);
	}
	
	void draw_midpath_points(map::Point2d_t &p);
	
	void draw_terrain(map::Point2d_t &p);
	
	void draw_midpath();
	
	template<typename Loc>
	void draw_pathpoint(const Loc &pi);
	
	void draw_path();
	
	int draw_terrain();
	
	int draw_map();
	
	
	WINDOW *draw_window(int height, int width, int xc, int yc);
	
	int draw_beam(p2D_t &po, p2D_t &pbeam);
	
	Map_Drawer(std::string &fdir, int d_);
	
};


int can_do_move(int x, int y);


#endif //OPENCVTEST_MAPDRAW_H
