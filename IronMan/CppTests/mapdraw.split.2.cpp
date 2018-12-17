
/* quest.c */


#include <curses.h>
//#include <ncurses.h>
#include <stdlib.h>
#include <fstream>
#include <iostream>
#include <unistd.h>
#include <sstream>

#define elif else if

#define GRASS       ' '
#define EMPTY       '.'
#define WATER       '~'
#define MOUNTAIN    '^'
#define FOREST      '&'
#define PLAYER      '*'
#define START       'S'
#define WIN         'W'
#define ROAD        '='

#define COLOR_BROWN 8
#define COLOR_ORANGE 9
#define COLOR_GREY 10

#define EMPTY_PAIR     1
#define PLAYER_PAIR    10

#define RGB255(x) short(x/0.255)

#define PATH_MAX 1000


enum terrain_types {
	road_e = 1000,
	trail_e = 1100,
	grass_e = 1200,
	water_e = 1300,
	rockywater_e = 1400,
	forest_e = 1500,
	start_e = 1600,
	win_e = 1700,
	null_terrain_e = 0,
};

enum terrain_types_ncurses {
	grass_nc = 1,
	trail_nc = 2,
	road_nc = 3,
	water_nc = 4,
	rockywater_nc = 5,
	forest_nc = 6,
	start_nc = 7,
	win_nc = 8,
	corner_nc = 9,
};

int is_move_okay(int y, int x);

void draw_map(int x, int y, int maPart);


void custom_color(short id, short r, short g, short b) {
	init_color(id, RGB255(r), RGB255(g), RGB255(b));
	
}

int init_colors() {
	/* initialize colors */
	
	if (has_colors() == FALSE) {
		endwin();
		printf("Your terminal does not support color\n");
		exit(1);
	}
	
	
	start_color();
	
	custom_color(COLOR_BROWN, 153, 102, 51);
	custom_color(COLOR_ORANGE, 255, 153, 0);
	custom_color(COLOR_GREY, 128, 128, 128);
	
	init_pair(grass_nc, COLOR_YELLOW, COLOR_GREEN);
	init_pair(road_nc, COLOR_WHITE, COLOR_GREY);
	init_pair(trail_nc, COLOR_ORANGE, COLOR_BROWN);
	init_pair(water_nc, COLOR_CYAN, COLOR_BLUE);
	init_pair(rockywater_nc, COLOR_BLACK, COLOR_BLUE);
	init_pair(forest_nc, COLOR_BLACK, COLOR_GREEN);
	init_pair(start_nc, COLOR_BLACK, COLOR_WHITE );
	init_pair(win_nc, COLOR_BLACK, COLOR_WHITE);
	init_pair(corner_nc, COLOR_WHITE, COLOR_BLACK);
	init_pair(PLAYER_PAIR, COLOR_RED, COLOR_MAGENTA);
	
	
	clear();
	return 0;
}

void init_mapper() {
	/* initialize curses */
	
	initscr();
	keypad(stdscr, TRUE);
	cbreak();
	noecho();
	
	
}


int main(void) {
	int y, x;
	int yPrev = -1, xPrev = -1;
	
	int ch;
	
	init_mapper();
	init_colors();
	
	//return 0;
	/* start player at lower-left */
	/* initialize the quest map */
	
	y = 47;
	x = 7;
	
	int mapPart = 0;
	
	if (y >= 49) mapPart = 1;
	
	draw_map(x, y, mapPart);
	
	do {
		
		/* by default, you get a blinking cursor - use it to
		   indicate player * */

		

		
		draw_map(x, y, mapPart);
		
		attron(COLOR_PAIR(PLAYER_PAIR));
		mvaddch(y, x, PLAYER);
		attroff(COLOR_PAIR(PLAYER_PAIR));
		move(y, x);
		
		if (y > 48) {
			mapPart = 1;
			y = 1;
		} elif (y < 1) {
			mapPart = 0;
			y = 48;
		};
		
		
		
		std::string xstr = std::to_string(x);
		std::string ystr = std::to_string(y);
		
		attron(COLOR_PAIR(corner_nc));
		mvhline(0, 108, 'P', 1);
		mvhline(0, 109, 'o', 1);
		mvhline(0, 110, 's', 1);
		mvhline(0, 111, 'i', 1);
		mvhline(0, 112, 't', 1);
		mvhline(0, 113, 'i', 1);
		mvhline(0, 114, 'o', 1);
		mvhline(0, 115, 'n', 1);
		mvhline(0, 116, 'n', 1);
		
		mvhline(1, 108, 'x', 1);
		mvhline(1, 109, ':', 1);
		mvhline(1, 110, xstr[0], 1);
		mvhline(1, 111, xstr[1], 1);
		
		mvhline(2, 108, 'y', 1);
		mvhline(2, 109, ':', 1);
		mvhline(2, 110, ystr[0], 1);
		mvhline(2, 111, ystr[1], 1);
		attroff(COLOR_PAIR(corner_nc));
		
		refresh();
		
		ch = getch();
		
		/* test inputted key and determine direction */
		
		switch (ch) {
			case KEY_UP:
			case 'w':
			case 'W':
				if ((y >= 0) && is_move_okay(y - 1, x)) {
					attron(COLOR_PAIR(corner_nc));
					mvhline(2, 110, 'o', 1);
					mvhline(2, 111, 'k', 1);
					attroff(COLOR_PAIR(corner_nc));
					attron(COLOR_PAIR(EMPTY_PAIR));
					mvaddch(y, x, EMPTY);
					attroff(COLOR_PAIR(EMPTY_PAIR));
					y = y - 1;
				} else {
					attron(COLOR_PAIR(corner_nc));
					mvhline(2, 110, 'n', 1);
					mvhline(2, 111, 'o', 1);
					attroff(COLOR_PAIR(corner_nc));
				 }
				break;
			case KEY_DOWN:
			case 's':
			case 'S':
				if ((y < LINES - 1) && is_move_okay(y + 1, x)) {
					attron(COLOR_PAIR(EMPTY_PAIR));
					mvaddch(y, x, EMPTY);
					attroff(COLOR_PAIR(EMPTY_PAIR));
					y = y + 1;
				}
				break;
			case KEY_LEFT:
			case 'a':
			case 'A':
				if ((x > 0) && is_move_okay(y, x - 1)) {
					attron(COLOR_PAIR(EMPTY_PAIR));
					mvaddch(y, x, EMPTY);
					attroff(COLOR_PAIR(EMPTY_PAIR));
					x = x - 1;
				}
				break;
			case KEY_RIGHT:
			case 'd':
			case 'D':
				if ((x < COLS - 1) && is_move_okay(y, x + 1)) {
					attron(COLOR_PAIR(EMPTY_PAIR));
					mvaddch(y, x, EMPTY);
					attroff(COLOR_PAIR(EMPTY_PAIR));
					x = x + 1;
				}
				break;
		}
		/* initialize the quest map */

		
	} while ((ch != 'q') && (ch != 'Q'));
	
	endwin();
	
	exit(0);
}

int is_move_okay(int y, int x) {
	unsigned long testch;
	
	/* return true if the space is okay to move into */
	
	testch = mvinch(y, x);
	bool canGo = ((testch & A_CHARTEXT) == GRASS)
	         || ((testch & A_CHARTEXT) == EMPTY)
	         || ((testch & A_CHARTEXT) == WATER)
	         || ((testch & A_CHARTEXT) == ROAD);
	
	return canGo;
}

void draw_point(int c, int x, int y) {
	switch (c) {
		case grass_e : {
			attron(COLOR_PAIR(grass_nc));
			mvhline(y, x, GRASS, 1);
			attroff(COLOR_PAIR(grass_nc));
			return;
		}
		case rockywater_e : {
			attron(COLOR_PAIR(rockywater_nc));
			mvhline(y, x, MOUNTAIN, 1);
			attroff(COLOR_PAIR(rockywater_nc));
			return;
		}
		case water_e : {
			attron(COLOR_PAIR(water_nc));
			mvhline(y, x, WATER, 1);
			attroff(COLOR_PAIR(water_nc));
			return;
		}
		case road_e : {
			attron(COLOR_PAIR(road_nc));
			mvhline(y, x, ROAD, 1);
			attroff(COLOR_PAIR(road_nc));
			return;
		}
		case trail_e : {
			attron(COLOR_PAIR(trail_nc));
			mvhline(y, x, ROAD, 1);
			attroff(COLOR_PAIR(trail_nc));
			return;
		}
		case forest_e : {
			attron(COLOR_PAIR(forest_nc));
			mvhline(y, x, FOREST, 1);
			attroff(COLOR_PAIR(forest_nc));
			return;
		}
		case start_e : {
			attron(COLOR_PAIR(start_nc));
			mvhline(y, x, START, 1);
			attroff(COLOR_PAIR(start_nc));
			return;
		}
		case win_e : {
			attron(COLOR_PAIR(win_nc));
			mvhline(y, x, WIN, 1);
			attroff(COLOR_PAIR(win_nc));
			return;
		}
		default:
			return;
	}
}

int yPrev = -1;

void draw_map(int xP, int yP, int mapPart) {
	clear();
	
	
	/* draw the quest map */
	
	/* background */
	
	std::string path = "../datadir/";
	std::string mapdataFname = "mapdata.static.csv";
	std::string dyndataFname = "mapdata.dynamic.csv";
	std::ifstream infile(path + mapdataFname);
	
	
	int c, l, x, y;
	
	l = 0;
	while (infile >> c) {
		
		y = l / 100; //col
		
		if (mapPart == 1){
			y -= 48;
		}
		
		x = l % 100; //row
		
		if (y >= 0 && y < 50){
			draw_point(c, x, y);
		}
		
		l++;
	}
	
	attron(COLOR_PAIR(corner_nc));
	mvhline(0, 0, 'A', 1);
	attroff(COLOR_PAIR(corner_nc));
	
	attron(COLOR_PAIR(corner_nc));
	mvhline(0, 100, 'B', 1);
	attroff(COLOR_PAIR(corner_nc));
	
	attron(COLOR_PAIR(corner_nc));
	mvhline(49, 100, 'C', 1);
	attroff(COLOR_PAIR(corner_nc));
	
	attron(COLOR_PAIR(corner_nc));
	mvhline(49, 0, 'D', 1);
	attroff(COLOR_PAIR(corner_nc));
	
	
}
