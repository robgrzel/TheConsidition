def ipython_info():
    import sys
    ip = False
    if 'ipykernel' in sys.modules:
        ip = True  # 'notebook'
    # elif 'IPython' in sys.modules:
    #    ip = 'terminal'
    return ip


ISIPYTHON = ipython_info()
import json

from datetime import datetime


def logwrite(logfile, txt):
    timeStr = datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]
    msg = '[%s] : %s \n' % (timeStr, txt)
    logfile.write(msg)


from api import API
import random
import sys
from draw_map import draw_map
from mapdata_parser import *

from pprint import pprint as ppr

import time

from queue import Queue
import numpy as np


class SquareGrid:

    def __init__(self, w, h):
        self.width = w
        self.height = h

    def in_bounds(self, id):
        (x, y) = id
        return 0 <= x < self.width and 0 <= y < self.height

    def neighbord_check(self, results, id):
        (x, y) = id
        if (x + y) % 2 == 0: results.reverse()  # aesthetics
        results = filter(self.in_bounds, results)
        return results

    def neighbors4(self, id):
        (x, y) = id
        results = [(x + 1, y + 0), (x + 0, y - 1), (x - 1, y + 0), (x + 0, y + 1)]
        results = self.neighbord_check(results, id)
        return results

    def neighbors8(self, id):
        (x, y) = id
        results = [(x + 1, y + 0), (x + 1, y - 1), (x, y - 1), (x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        results = self.neighbord_check(results, id)
        return results


def floof_fill(graph, start, goal, MAP):
    frontier = Queue()
    frontier.put(start)
    came_from = {}
    came_from[start] = None
    print('start ...')
    cnt = 0

    PMAP = np.zeros([100, 100])

    while not frontier.empty():
        current = frontier.get()

        if current == goal:
            break

        for next in graph.neighbors4(current):
            x, y = next
            cnt += 1
            if cnt % 1000 == 0: print(cnt)
            if MAP[y, x] == point_types['nowaypoint']:
                continue
            else:
                if PMAP[y][x] == 0:
                    # if next not in came_from:
                    PMAP[y][x] = 1
                    frontier.put(next)
                    came_from[next] = current

    print('end...', cnt)
    return came_from


def traverse_back(goal, start, came_from):
    current = goal
    path = []
    while current != start:
        path.append(current)
        current = came_from[current]
    path.append(start)  # optional
    path.reverse()  # optional
    return path


# A "solution" that takes a step in a random direction every turn
def solution(game_id, logfile, stateFname, mapdataFname, path):
    gotMap = False
    initial_state = _api.get_game(game_id)
    if (initial_state["success"] == True):
        state = initial_state["gameState"]
        tiles = state["tileInfo"]
        current_player = state["yourPlayer"]
        current_y_pos = current_player["yPos"]
        current_x_pos = current_player["xPos"]
        while not state["gameStatus"] == "done":
            t1 = time.time()
            logwrite(logfile, "Starting turn: " + str(state["turn"]))
            tiles = state["tileInfo"]
            logwrite(logfile, str(state["yourPlayer"]))
            current_player = state["yourPlayer"]
            current_y_pos = current_player["yPos"]
            current_x_pos = current_player["xPos"]
            # Take a step in a random direction
            step_direction_array = ["w", "e", "n", "s"]
            random_step = random.randint(0, 3)
            logwrite(logfile, "Stepped: " + str(step_direction_array[random_step]))
            response = _api.step(game_id, step_direction_array[random_step])
            state = response["gameState"]

            with open('%s/%s.json' % (path, stateFname), 'w') as outfile:
                json.dump(state, outfile)

            t2 = time.time()

            if gotMap == False:
                gotMap = True
                mapdynamic, mapterrain = mapdata_parser(path, stateFname, mapdataFname, doOnlyDyn=False, doSkeletonizeStatic=False)



            else:
                mapdynamic = mapdata_parser(path, stateFname, mapdataFname, doOnlyDyn=True)

            t3 = time.time()

            print(t2 - t1, t3 - t1)

        # draw_map(state)

        logwrite(logfile, "Finished!")
    else:
        logwrite(logfile, initial_state["message"])


gameId = None


def ironman(_api, path, stateFname, mapdataFname, logfile, gameId=None):
    logfile.write("start ironman with gameId : %s" % str(gameId))
    # If no gameID is specified as parameter to the script,
    # Initiate a game with 1 player on the standard map
    if (gameId is None):
        _api.end_previous_games_if_any()  # Can only have 2 active games at once. This will end any previous ones.
        gameId = _api.init_game()
        gameIdFile = open("gameid.txt", 'w')
        gameIdFile.write(gameId)
        gameIdFile.close()
        ironman(_api, path, stateFname, mapdataFname, logfile, gameId)
    else:
        joinedGame = _api.join_game(gameId)
        readiedGame = _api.try_ready_for_game(gameId)
        if (readiedGame != None):
            logwrite(logfile, "Joined and readied! Solving...")
            solution(gameId, logfile, stateFname, mapdataFname, path)


if __name__ == "__main__":
    path = '../datadir'
    logFname = 'ironman.log'
    mapdataFname = 'mapdata'
    stateFname = 'statedata'

    # Insert your API-key here
    _api_key = "326dcc2e-6ccc-457a-bd12-596eadc87a64"
    # Specify your API-key number of players per game),
    # mapname, and number of waterstreams/elevations/powerups here

    with open("%s/%s.txt" % (path, logFname), "w") as logfile:
        _api = API(_api_key, 1, "standardmap", 10, 10, 10, logfile)
        ironman(_api, path, stateFname, mapdataFname, logfile, )
