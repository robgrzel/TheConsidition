#pragma once

// Example program

#ifndef GRAFS_HPP
#define GRAFS_HPP

// uncomment to disable assert()
// #define NDEBUG




#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <tuple>

#include <iostream>
#include <algorithm>
#include <queue>

#include <cassert>
#include <cstdio>
#include <string.h>


#define fori(s, e) for(int i=s; i<e; i++)
#define forj(s, e) for(int j=s; j<e; j++)

#define elif else if


typedef struct p2D {
	int x;
	int y;
	int id;
	
	bool None = false;
	
	int getx() const { return x; }
	
	int gety() const { return y; }
	
	bool operator==(const p2D &p) const {
		return (x == p.x) && (y == p.y);
	}
	
	bool operator!=(const p2D &p) const {
		return (x != p.x) || (y != p.y);
	}
	
	bool operator<(const p2D &b) {
		return std::tie(x, y) < std::tie(b.x, b.y);
	}
	
	bool operator<=(const p2D &b) {
		return std::tie(x, y) <= std::tie(b.x, b.y);
	}
	
	bool operator>(const p2D &b) {
		return std::tie(x, y) > std::tie(b.x, b.y);
	}
	
	bool operator>=(const p2D &b) {
		return std::tie(x, y) >= std::tie(b.x, b.y);
	}
	
	friend std::basic_iostream<char>::basic_ostream &operator<<
			(std::basic_iostream<char>::basic_ostream &out, const p2D &loc) {
		out << '(' << loc.x << ',' << loc.y << ')';
		return out;
	}
	
} p2D_t;


namespace std {
	/* implement hash function so we can put GridLocation into an unordered_set */
	
	template<>
	struct hash<p2D_t> {
		typedef p2D_t argument_type;
		typedef std::size_t result_type;
		
		std::size_t operator()(const p2D_t &id) const noexcept {
			return std::hash<int>()(id.x ^ (id.y << 4));
			//return ((hash<int>()(id.x) ^ (hash<int>()(id.y) << 1)) >> 1);
			
		}
	};
	
}

static const p2D_t dirs4[4] = {{+1, +0},
                               {+0, -1},
                               {-1, +0},
                               {+0, +1}};

static const p2D_t dirs8[8] = {{+1, +0},
                               {+0, -1},
                               {-1, +0},
                               {+0, +1}};

namespace gr {
	
	typedef enum {
		NN4_e = 4,
		NN8_e = 8,
	} NN_t;
	
	
	template<unsigned nn>
	class Square_Grid {
	private:
		p2D_t pmin;
		p2D_t pmax;
		p2D_t NN[nn];
		p2D_t NN_[nn];
		int nncnt = nn;
	
	public:
		
		Square_Grid(const p2D_t &pmin_, const p2D_t &pmax_) {
			
			assert (pmin_.x < pmax_.x);
			assert (pmin_.y < pmax_.y);
			
			pmin = pmin_;
			pmax = pmax_;
		}
		
		int get_neighbors(p2D_t *out) {
			fori(0, nncnt) {
				out[i] = NN[i];
			}
			return nncnt;
		}
		
		bool in_bounds(p2D_t p) {
			
			return (pmin.x <= p.x) && (p.x < pmax.x) &&
			       (pmin.y <= p.y) && (p.y < pmax.y);
			
		}
		
		int neighbors_check(p2D_t p, bool doShortest) {
			
			//we reverse to get shortest paths
			//otherwise they will be straight
			
			if (doShortest) {
				
				bool doReverse = (p.x + p.y) % 2 == 0;
				
				if (doReverse) {
					fori(0, nn) NN_[i] = NN[nn - 1 - i];
					fori(0, nn) NN[i] = NN_[i];
					
				}
			}
			
			bool inBounds;
			nncnt = 0;
			
			fori(0, nn) {
				inBounds = in_bounds(NN[i]);
				
				if (inBounds) {
					NN_[nncnt++] = NN[i];
				}
			}
			
			
			fori(0, nncnt) {
				NN[i] = NN_[i];
			}
			
			return nncnt;
		}
		
		/*nn.erase(
			std::remove_if(nn.begin(), nn.end(), std::bind1st(std::mem_fun(&Square_Grid::in_bounds), this)),
			nn.end()
			);
		*/
		
		
		int neighbors(p2D_t p, p2D_t out[4], bool doShortest) {
			int x = p.x;
			int y = p.y;
			bool notInBorder = !in_bounds(p);
			
			if (notInBorder) return 0;
			
			if (nn == 4) fori(0, nn) NN[i] = {p.x + dirs4[i].x, p.y + dirs4[i].y};
			if (nn == 8) fori(0, nn) NN[i] = {p.x + dirs8[i].x, p.y + dirs8[i].y};
			
			int k = neighbors_check(p, doShortest);
			
			
			fori(0, k) out[i] = NN[i];
			
			return k;
			
		}
		

		
		
	};
	
	
	template<unsigned N, unsigned M, unsigned nn, template <unsigned> typename Graf_t, typename Loc_t>
	class Graf_Traversal {
	private:
		
		bool PATHMAP[N][M];
		
		Loc_t NN[nn];
		
		Graf_t<nn> Graf;
		
		std::queue<Loc_t> frontier;
	
	public:
		
		int bfscnt = 0;
		std::vector<Loc_t> pathto;
		std::unordered_map<Loc_t, Loc_t> pathfrom;
		
		
		Graf_Traversal() : Graf({0, 0}, {N, M}) {
			
			pathto.clear();
			pathfrom.clear();
			memset(PATHMAP, 0, sizeof(PATHMAP));
			
		}
		
		Graf_Traversal(const Loc_t &pmin, const Loc_t &pmax) : Graf(pmin, pmax) {
			
			pathto.clear();
			pathfrom.clear();
			memset(PATHMAP, 0, sizeof(PATHMAP));
			
		}
		
		int bfs_from_to(const Loc_t &start, const Loc_t &goal, int (*filter)(const Loc_t &), bool doShortest) {
			
			pathfrom.clear();
			
			bfs_flood_fill(start, goal, filter, doShortest);
			traverse_back(start, goal);
			
			return bfscnt;
			
		}
		
		
		int bfs_flood_fill(const Loc_t &start, const Loc_t &goal, int (*filter)(const Loc_t &), bool doShortest) {
			bfscnt = 0;
			int nni = 0;
			Loc_t next;
			Loc_t current;
			frontier.push(start);
			pathfrom[start] = start;
			
			while (not frontier.empty()) {
				current = frontier.front();
				frontier.pop();
				
				if (current == goal) break;
				nni = Graf.neighbors(current, NN, doShortest);
				fori(0, nni) {
					bfscnt ++;
					//if (cnt++ % 1000 == 0) printf("...bfs:%d\n", cnt);
					next = NN[i];
					//if (MAP[next.y][next.x] == 0) continue;
					if ((*filter)(next)) continue;
					if (PATHMAP[next.x][next.y] == 0) {
						PATHMAP[next.x][next.y] = 1;
						pathfrom[next] = current;
						frontier.push(next);
					}
				}
			}
			std::queue<Loc_t>().swap(frontier);
			memset(PATHMAP, 0, sizeof(PATHMAP)); // for automatically-allocated arrays
			
			return bfscnt;
			
		}
		
		int traverse_back(const Loc_t &start, const Loc_t &goal) {
			Loc_t current = goal;
			
			pathto.clear();
			
			while (current != start) {
				bfscnt++;
				pathto.push_back(current);
				current = pathfrom.at(current);
			}
			
			pathto.push_back(start);
			std::reverse(pathto.begin(), pathto.end());
			
			return bfscnt;
			
		}
		
		
		int pathfrom_to_file(std::string fdir) {
			FILE *fp;
			
			/* open the file for writing*/
			fp = fopen((fdir + "/datadir/" + "pathfrom.csv").data(), "w");
			
			/* write 10 lines of text into the file stream*/
			for (auto &i : pathfrom) {
				fprintf(fp, "%d,%d,%d,%d\n", i.first.x, i.first.y, i.second.x, i.second.y);
				
			}
			
			/* close the file*/
			fclose(fp);
			return 0;
		}
		
		
		int path_start_win_to_file(std::string fdir) {
			FILE *fp;
			
			/* open the file for writing*/
			fp = fopen((fdir + "/datadir/" + "pathstartwin.csv").data(), "w");
			
			/* write 10 lines of text into the file stream*/
			for (auto &i : pathto) {
				fprintf(fp, "%d,%d\n", i.x, i.y);
				
			}
			
			/* close the file*/
			fclose(fp);
			return 0;
		}
	};
	
	
}


#endif


