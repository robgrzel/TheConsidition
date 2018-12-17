#include <ncurses.h>
#include <string.h>	/* facilitate strlen() */

#define WHITEONRED 1
#define WHITEONBLUE 2
#define WHITEONBLACK 3
#define BLACKONWHITE 4


void wCenterTitle(WINDOW *pwin, const char * title)
{
	int x, maxy, maxx, stringsize;
	getmaxyx(pwin, maxy, maxx);
	stringsize = 4 + strlen(title);
	x = (maxx - stringsize)/2;
	mvwaddch(pwin, 0, x, ACS_RTEE);
	waddch(pwin, ' ');
	waddstr(pwin, title);
	waddch(pwin, ' ');
	waddch(pwin, ACS_LTEE);
}

void wclrscr(WINDOW * pwin)
{
	int y, x, maxy, maxx;
	getmaxyx(pwin, maxy, maxx);
	for(y=0; y < maxy; y++)
		for(x=0; x < maxx; x++)
			mvwaddch(pwin, y, x, ' ');
}

int main(int c, char *argv[])
{
	WINDOW *base_win, *small_win;
	int maxy, maxx;
	
	/* INITIALIZE CURSES AND COLORS AND REFRESH THE STANDARD SCREEN */
	initscr();
	getmaxyx(stdscr, maxy, maxx);
	start_color();
	init_pair(WHITEONRED, COLOR_WHITE, COLOR_RED);
	init_pair(WHITEONBLUE, COLOR_WHITE, COLOR_BLUE);
	init_pair(WHITEONBLACK, COLOR_WHITE, COLOR_BLACK);
	init_pair(BLACKONWHITE, COLOR_BLACK, COLOR_WHITE);
	wrefresh(stdscr); /* I don't know why this is necessary, but it is! */
	
	/* CREATE AND DISPLAY THE BASE WINDOW */
	base_win = newwin(maxy, maxx, 0,0);
	wattrset(base_win, COLOR_PAIR(WHITEONBLUE) | WA_BOLD);
	wclrscr(base_win);
	box(base_win, 0, 0);
	wCenterTitle(base_win, "Large Window");
	mvwaddstr(base_win, 7,6,"Press enter to continue, if you please==>");
	touchwin(base_win);
	wrefresh(base_win);
	getch();
	
	/* CREATE AND DISPLAY THE SMALL WINDOW */
	small_win = newwin(10, 30, 3,3);
	wattrset(small_win, COLOR_PAIR(WHITEONRED) | WA_BOLD);
	wclrscr(small_win);
	box(small_win, 0, 0);
	wCenterTitle(small_win, "Small Window");
	mvwaddstr(small_win, 1,1,"small win");
	mvwaddstr(small_win, 2,2,"small win");
	mvwaddstr(small_win, 3,3,"Enter=continue=>");
	touchwin(small_win);
	wrefresh(small_win);
	getch();
	
	/* BURY SMALL WINDOW */
	touchwin(base_win);
	wrefresh(base_win);
	getch();
	
	/* UNBURY SMALL WINDOW */
	touchwin(small_win);
	wrefresh(small_win);
	getch();
	
	/* DELETE SMALL WINDOW */
	delwin(small_win);
	touchwin(base_win);
	wrefresh(base_win);
	getch();
	
	/* END CURSES */
	endwin();
	printf("\n\nwin.c\n");
	return(0);
}