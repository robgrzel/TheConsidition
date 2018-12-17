


#include "mapdraw.hpp"


/* quest.c */

namespace nc {
	
	//#include <ncurses.h>
	
	void init_mapper() {
		/* initialize curses */
		
		initscr();
		keypad(stdscr, TRUE);
		cbreak();
		noecho();
		
		
	}
	
	void draw_char(int x, int y, map::terrain_types_nc_e t) {
		if (map::signs_nc.find(t) == map::signs_nc.end()) return;
		
		attron(COLOR_PAIR(t));
		mvaddch(y, x, map::signs_nc[t]);
		attroff(COLOR_PAIR(t));
		
	}
	
	
	void draw_hline(int x, int y, int cols, int t) {
		if (map::signs_nc.find(t) == map::signs_nc.end()) return;
		attron(COLOR_PAIR(t));
		mvhline(y, x, map::signs_nc[t], cols);
		attroff(COLOR_PAIR(t));
	}
	
	int draw_lines(map::terrain_types_nc_e t, int n, const int line[][2]) {
		//attron(COLOR_PAIR(t));
		int x, y;
		fori(0, n) {
			x = line[i][0];
			y = line[i][1];
			mvaddch(y, x, map::signs_nc[t]);
		}
		//attroff(COLOR_PAIR(t));
		return 0;
	}
	
	int draw_htext(int x, int y, map::terrain_types_nc_e t, const std::string &txt) {
		attron(COLOR_PAIR(t));
		int h = x;
		for (auto c: txt) {
			mvhline(y, h++, c, 1);
		}
		attroff(COLOR_PAIR(t));
		return h + 1;
	}
	
	void draw_vtext(int x, int y, map::terrain_types_nc_e t, const std::string &txt) {
		attron(COLOR_PAIR(t));
		int h = y;
		for (auto c: txt) {
			mvhline(h++, x, c, 1);
		}
		attroff(COLOR_PAIR(t));
		
	}
	
	
	void custom_color(short id, short r, short g, short b) {
		init_color(id, RGB255(r), RGB255(g), RGB255(b));
		
	}
	
	
}


int can_do_move(int x, int y) {
	unsigned long testch;
	
	/* return true if the space is okay to move into */
	
	testch = mvinch(y, x);
	
	for (auto t : map::cango_terrain_signs_nc) {
		if ((testch & A_CHARTEXT) == t) {
			return true;
		}
	}
	
	return false;
}


int init_colors() {
	/* initialize colors */
	
	if (has_colors() == FALSE) {
		endwin();
		printf("Your terminal does not support color\n");
		exit(1);
	}
	
	
	start_color();
	
	nc::custom_color(COLOR_BROWN, 153, 102, 51);
	nc::custom_color(COLOR_ORANGE, 255, 153, 0);
	nc::custom_color(COLOR_GREY, 128, 128, 128);
	
	init_pair(map::grass_nc, COLOR_YELLOW, COLOR_GREEN);
	init_pair(map::road_nc, COLOR_WHITE, COLOR_GREY);
	init_pair(map::trail_nc, COLOR_ORANGE, COLOR_BROWN);
	init_pair(map::water_nc, COLOR_CYAN, COLOR_BLUE);
	init_pair(map::rockywater_nc, COLOR_BLACK, COLOR_BLUE);
	init_pair(map::forest_nc, COLOR_BLACK, COLOR_GREEN);
	init_pair(map::start_nc, COLOR_BLACK, COLOR_WHITE);
	init_pair(map::win_nc, COLOR_BLACK, COLOR_WHITE);
	init_pair(map::corner_nc, COLOR_WHITE, COLOR_BLACK);
	init_pair(map::midpoint_nc, COLOR_WHITE, COLOR_WHITE);
	init_pair(map::pathpoint_nc, COLOR_CYAN, COLOR_WHITE);
	init_pair(map::player_nc, COLOR_RED, COLOR_MAGENTA);
	init_pair(map::beam_nc, COLOR_RED, COLOR_BLUE);
	
	clear();
	return 0;
}

void Map_Drawer::update_pdraw() {
	pdraw = {p.x - pmin.x, p.y - pmin.y};
}

int Map_Drawer::read_map_terrain(bool doSaveCSV) {
	map::read_map_static(fdir);
	map::get_start_and_end();
	map::get_waypoints();
	map::get_midpath();
	
	if (doSaveCSV) {
		map::startend_to_file(fdir);
		map::waydata_to_file(fdir);
		map::skeleton_to_file(fdir);
		map::midpoints_to_file(fdir);
	}
	
	
}

int Map_Drawer::find_terrain_path(p2D_t start, p2D_t end) {
	pstart = {start.x, start.y};
	pgoal = {end.x, end.y};
	
	p = start;
	
	Graf = gr::Graf_Traversal<
			map::NROWS,
			map::NCOLS,
			gr::NN4_e,
			gr::Square_Grid,
			p2D_t>();
	
	Graf.bfs_from_to(pstart, pgoal, map::filter_noways, false);
	pnext = Graf.pathto[0];
}

int Map_Drawer::run_simulation() {
	std::cout << "visible distance = " << maxview << std::endl;
	
	read_map_terrain(false);
	
	//find_terrain_path(map::pstart, map::pend);
	
	pstart = {map::pstart.x, map::pstart.y};
	pgoal = {map::pend.x, map::pend.y};
	
	p = pstart;
	
	Graf = gr::Graf_Traversal<
			map::NROWS,
			map::NCOLS,
			gr::NN4_e,
			gr::Square_Grid,
			p2D_t>();
	
	Graf.bfs_from_to(pstart, pgoal, map::filter_noways, false);
	pnext = Graf.pathto[0];
	
	nc::init_mapper();
	init_colors();
	
	move_pstart(pstart);
	update_lims();
	update_pdraw();
	
	int ch;
	
	do {
		
		pprev = p;
		
		draw_map();
		show_info();
		
		update_pdraw();
		update_lims();
		
		nc::draw_char(pdraw.x, pdraw.y, map::player_nc);
		
		move(pdraw.y, pdraw.x);
		
		refresh();
		
		ch = ERR;
		
		if (win_resize_handler()) {
		
		} else {
			
			timeout(1);
			ch = getch();
			
			if (ch != ERR) {
				keys_handler(ch);
				
			} else {
				
				//int isPathEnd = refresh_path(true);
				//if (isPathEnd) swap_start_goal();
				
				//Graf.bfs_from_to(p,pgoal,map::filter_noways, false);
				if (1 < Graf.pathto.size()) {
					movedir = 't';
					auto pi = Graf.pathto[1];
					pnext = pi;
					
				} else  swap_start_goal();
				
				movexy(pnext.x, pnext.y);
 			}
			
			nc::draw_char(p.x, p.y, map::empty_nc);
		}
		
		
	} while ((ch != 'q') && (ch != 'Q'));
	
	endwin();
}

Map_Drawer::Map_Drawer(std::string &fdir, int d) : maxview(d), fdir(fdir) {

}

int Map_Drawer::keys_handler(int ch) {
	switch (ch) {
		case KEY_UP:
		case 'w':
		case 'W':
			moveup();
			break;
		case KEY_DOWN:
		case 's':
		case 'S':
			movedn();
			break;
		case KEY_LEFT:
		case 'a':
		case 'A':
			movelt();
			break;
		case KEY_RIGHT:
		case 'd':
		case 'D':
			movert();
			break;
		default:
			
			return 1;
	}
	
	
	
	return 0;
}

int Map_Drawer::move_pstart(p2D_t pi) {
	if (in_border(pi.x, pi.y) && can_do_move(pi.x, pi.y)) {
		nc::draw_char(pi.x, pi.y, map::empty_nc);
		pprev.x = pi.x;
		pprev.y = pi.y;
		p.x = pi.x;
		p.y = pi.y;
		
	}
	
}

int Map_Drawer::movep(p2D_t p) {
	movexy(p.x, p.y);
}

int Map_Drawer::movexy(int x, int y) {
	
	int dir = -1;
	int xi, yi;
	
	if (p.x == x and p.y == y) {
		return 0;
	} else {
		fori(0, 4) {
			xi = p.x + dirs4[i].x;
			yi = p.y + dirs4[i].y;
			
			if (x == xi and y == yi) {
				dir = i;
				break;
			}
		}
	}
	
	switch (dir) {
		case 0:
			keys_handler('D');
			break;
		case 1:
			keys_handler('S');
			break;
		case 2:
			keys_handler('A');
			break;
		case 3:
			keys_handler('W');
			break;
		default:
			return -1;
			//std::this_thread::sleep_for(std::chrono::seconds(10));
			//throw std::invalid_argument("wrong direction!");
		
	}
	return 0;
}

int Map_Drawer::in_border(int x, int y) {
	return (0 <= x && x < COLS - 1) && (0 <= y && y < LINES - 1);
}

int Map_Drawer::moveup() {
	
	if ((pdraw.y < LINES) && can_do_move(pdraw.x, pdraw.y + 1)) {
		nc::draw_char(pdraw.x, pdraw.y, map::empty_nc);
		incy();
	}
}

int Map_Drawer::movedn() {
	
	if ((pdraw.y > 0) && can_do_move(pdraw.x, pdraw.y - 1)) {
		nc::draw_char(p.x, p.y, map::empty_nc);
		dcry();
	}
}

int Map_Drawer::movelt() {
	
	if ((pdraw.x > 0) && can_do_move(pdraw.x - 1, pdraw.y)) {
		nc::draw_char(pdraw.x, pdraw.y, map::empty_nc);
		dcrx();
	}
}

int Map_Drawer::movert() {
	
	
	if ((pdraw.x < COLS - 1) && can_do_move(pdraw.x + 1, pdraw.y)) {
		nc::draw_char(pdraw.x, pdraw.y, map::empty_nc);
		incx();
	}
}

int Map_Drawer::show_info() {
	std::string xstr = std::string("x:") + std::to_string(p.x);
	std::string ystr = std::string("y:") + std::to_string(p.y);
	std::string xprevstr = std::string("xp:") + std::to_string(pprev.x);
	std::string yprevstr = std::string("yp:") + std::to_string(pprev.y);
	
	
	int txtrow = 0;
	
	nc::draw_htext(101, txtrow++, map::corner_nc,
	               std::string("Pstart : {") + std::to_string(map::pstart.x) + ", " + std::to_string(map::pstart.y) + "}");
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Pend : {") + std::to_string(map::pend.x) + ", " + std::to_string(map::pend.y) + "}");
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Step ID : ") + std::to_string(pnext.id));
	
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Pnext : {") + std::to_string(pnext.x) + ", " + std::to_string(pnext.y) + "}");
	
	
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("xlim: ") + std::to_string(pmin.x) + "," + std::to_string(pmax.x));
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("ylim: ") + std::to_string(pmin.y) + "," + std::to_string(pmax.y));
	
	txtrow += 5;
	
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Movedir: ") + movedir);
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Goal: ") + std::to_string(pgoal.x) + "," + std::to_string(pgoal.y));
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Positions:"));
	nc::draw_htext(101, txtrow++, map::corner_nc, xstr + ", " + ystr);
	nc::draw_htext(101, txtrow++, map::corner_nc, xprevstr + ", " + yprevstr);
	
	txtrow += 5;
	
	nc::draw_htext(101, txtrow++, map::corner_nc, std::string("Path search OP: ") + std::to_string(Graf.bfscnt));
}

int Map_Drawer::win_resize_handler() {
	
	if (LINES != linesPrev) {
		linesPrev = LINES;
		return 1;
	}
	
	return 0;
	
}


void Map_Drawer::draw_midpath_points(map::Point2d_t &pix) {
	if (pix.pointType == map::midpoint_e)
		nc::draw_hline(pix.p.x, pix.p.y, 1, pix.pointType);
	
}

void Map_Drawer::draw_terrain(map::Point2d_t &pix) {
	
	if (map::signs_nc.find(pix.terrainTypeNc) != map::signs_nc.end())
		nc::draw_hline(pix.p.x, pix.p.y, 1, pix.terrainTypeNc);
	
}


template<typename Loc>
void Map_Drawer::draw_pathpoint(const Loc &pi) {
	
	auto pixel = map::make_pathpoint(pi.x - pmin.x, pi.y - pmin.y);
	
	draw_terrain(pixel);
	
}

void Map_Drawer::draw_path() {
	
	/*if (Graf == nullptr) return;//throw std::invalid_argument("Graph is nullptr");
	*/
	for (auto pi : Graf.pathto) {
		if (pmin.x <= pi.x && pi.x <= pmax.x)
			if (pmin.y <= pi.y && pi.y <= pmax.y)
				if (dist(pi, p) < maxview)
					draw_pathpoint(pi);
		
	}
}


void Map_Drawer::draw_midpath() {
	
	
	map::Point2d_t pixel;
	
	int c;
	
	int x, y;
	for (int i = pmin.x; i < pmax.x; i++) {
		for (int j = pmin.y; j < pmax.y; j++) {
			
			if (map::skeleton[i][j] == 0) continue;
			
			pixel.p = {i - pmin.x, j - pmin.y};
			
			pixel.pointType = map::midpoint_e;
			pixel.terrainType = map::null_terrain_e;
			c = map::midpoint_e;
			
			pixel.terrainTypeNc = map::midpoint_nc;
			
			draw_terrain(pixel);
			
		}
	}
	
	
}

int Map_Drawer::draw_terrain() {
	map::Point2d_t pixel;
	
	int c;
	
	for (int i = pmin.x; i < pmax.x; i++) {
		for (int j = pmin.y; j < pmax.y; j++) {
			
			pixel.p = {i - pmin.x, j - pmin.y};
			
			if (dist(pixel.p, pdraw) < maxview) {
				parse_pixel(&pixel, map::mapdata[i][j]);
				draw_terrain(pixel);
			}
		}
	}
	
	return 0;
}

int Map_Drawer::draw_map() {
	clear();
	
	
	nc::draw_hline(0, 0, 1, 'A');
	nc::draw_hline(100, 0, 1, 'B');
	nc::draw_hline(100, LINES, 1, 'C');
	nc::draw_hline(0, LINES, 1, 'D');
	
	/* draw the quest map */
	
	/* background */
	
	draw_terrain();
	
	//draw_midpath();
	
	draw_path();
	
	return 0;
}


int main(int argc, char *argv[]) {
	
	std::string fname = argv[0];
	std::string fdir = argv[1];
	
	int d;
	if (argc == 3) d = std::stoi(argv[2]);
	else d = 10;
	
	Map_Drawer(fdir, d).run_simulation();
	
	exit(0);
}

