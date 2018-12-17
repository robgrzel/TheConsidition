# TheConsidition

Implemented with python and c++. Building c++ part do not require any extra libs beside ncurses and terminal with 256 colors. Python and c++ are not seamlessly integrated, will be meant to work as separate server and client with data transfer by tcp socket.

As of now it is capable to show map with game, recompute optimal path at each player step and render player movement over this path. Usage is simple:

```bash

git clone https://github.com/robgrzel/TheConsidition
cd TheConsidition/IronMan

./TheConsidition.sh


```


# Status of integrated parts:

Python:
- using API to scrap game status (map terrain and dynamics)
- parse data according to some keymap
- save it to files
+ djiksta flood fill algorithm for path planning

Cpp:
- read data from files saved by python
- render map in real time in terminal with ncurses  
- represent map as bitmap with boolean values, 1 is passable point, 0 is blocked
- skip cost of terrain, dynamics and powerups
- djikstra flood fill algorithm for initial path search
- recompute path at each step with djikstra algorithm


# Todo:

Python: 
- tcp socket server sending data directly to cpp client

Cpp:
- tcp socket client receiving data from python server
- use djikstra path search for initial path finding only (with bitmap
- include costs in map and dynamics of terrain (no longer bitmap))
- split initial path at chunks with heurestic set as distance from start to end of chunk
- at each chunk use A* at each point to find optimal path
- include extra costs at A* 

# Possible optimizations:
- Use parallel djikstra algorithm with multiprocessing OpenMPI (nonblocking data transfer)


