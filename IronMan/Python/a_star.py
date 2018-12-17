# -*- coding: utf-8 -*-
"""A* implementation"""

import math
import sys
import webbrowser
import copy
from api import API
from draw_map import draw_map


MAP_SIZE = 100

MOVEMENT_POINT_FAST = 210
MOVEMENT_POINT_MEDIUM = 150
MOVEMENT_POINT_SLOW = 100

TILE_POINT_WATER = 45
TILE_POINT_ROAD = 31
TILE_POINT_TRAIL = 40
TILE_POINT_GRASS = 50
TILE_POINT_START_WIN = 30
TILE_POINT_TREE = 20
Tile_POINT_RAIN = 7

# Insert your API-key here
_api_key = "e7645575-8726-4abf-b75f-a1fe53604ce3"
# Specify your API-key number of players per game),
# mapname, and number of waterstreams/elevations/powerups here
_api = API(_api_key, 1, "southafricamap", 10, 10, 10)

# TODO: POWERUPPS
# TODO: Stå över en omgång för att vila?
# TODO: Ta omväg för att ta powerupp?
# TODO: Strömmar, kullar och regn

class Node():
    """A node class for A* Pathfinding"""

    def __init__(self, parent=None, position=None):
        self.parent = parent
        self.position = position

        self.g = 0
        self.h = 0
        self.f = 0

    def __eq__(self, other):
        return self.position == other.position


# Loopar igenom alla rutor och kollar efter start och slutnod
def find_start_and_finish(tiles):
    """Hittar start och slutnod"""
    start_and_finish = []
    for cord_y in range(99):
        for cord_x in range(99):
            tile = tiles[cord_y][cord_x]
            if (tile["type"] == "start" or tile["type"] == "win"):
                tile["cordx"] = cord_x
                tile["cordy"] = cord_y
                start_and_finish.append(tile)
    return start_and_finish

def in_range(node_position):
    """Kollar om noden är i kartan"""
    return node_position[0] > MAP_SIZE - 1 or node_position[0] < 0 or \
       node_position[1] > MAP_SIZE - 1 or node_position[1] < 0

def get_children(current_node, tiles):
    """Returnerar lista med alla barn"""
    children = []
    # for new_position in [(0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), \
    #                      (-1, 1), (1, -1), (1, 1)]: # Adjacent squares
    # Efterssom vi bara får gå i 4a riktningar:
    for new_position in [(0, -1), (0, 1), (-1, 0), (1, 0)]: # Adjacent squares

        # Get node position
        node_position = (current_node.position[0] + new_position[0],
                         current_node.position[1] + new_position[1])

        # Make sure within range
        if in_range(node_position):
            continue

        # Make sure walkable terrain
        tile = tiles[node_position[0]][node_position[1]]
        if tile["type"] == "forest" or tile["type"] == "rockywater":
            continue

        # Create new node
        new_node = Node(current_node, node_position)

        # Append
        children.append(new_node)
    return children


def astar(tiles, start, end):
    """Söker igenom och hittar snabbaste vägen returnerar en lista med cordinater"""
    # TODO: Algoritmen är inte 100% rätt.. Ibland tar den inte kortaste vägen..

    # Create start and end node
    start_node = Node(None, start)
    start_node.g = start_node.h = start_node.f = 0
    end_node = Node(None, end)
    end_node.g = end_node.h = end_node.f = 0

    # Initialize both open and closed list
    open_list = []
    closed_list = []

    # Add the start node
    open_list.append(start_node)

    # Loop until you find the end
    while open_list:
        # Get the current node
        current_node = open_list[0]
        current_index = 0
        # Get node with lowest f value
        for index, item in enumerate(open_list):
            if item.f < current_node.f:
                current_node = item
                current_index = index

        # Found the goal
        if current_node == end_node:
            print("Found the way!")
            path = []
            current = current_node
            while current is not None:
                path.append(current.position)
                current = current.parent
            return path[::-1] # Return reversed path

        # Pop current off open list, add to closed list
        open_list.pop(current_index)
        closed_list.append(current_node)


        # Generate children
        children = get_children(current_node, tiles)


        # Loop through children
        for child in children:
            # Child is on the closed list
            if child in closed_list:
                continue

            # Create the f, g, and h values
            child.g = current_node.g + 1
            child.h = (abs(child.position[0] - end_node.position[0]) +
                       abs(child.position[1] - end_node.position[1]))
            child.f = child.g + child.h

            # Child is already in the open list
            in_open = False
            for open_node in open_list:
                # open_node = open_list[index]
                if child.position == open_node.position:
                    if child.g < open_node.g:
                        # open_list[index] = child
                        open_list.append(child)
                    in_open = True
                    break

            # Add the child to the open list if not in open list
            if in_open is False:
                open_list.append(child)

#def look_ahead(tiles, current_tile):
#    pass
def stream_calc(move_direction, stream_direction, stream_speed):
    #medströms
    if stream_direction == move_direction:
        return -stream_speed

    #motströms
    elif((stream_direction == "n" and move_direction == "s") or
         (stream_direction == "s" and move_direction == "n") or
         (stream_direction == "e" and move_direction == "w") or
         (stream_direction == "w" and move_direction == "e")):
        return stream_speed


    #Nedan är bugg...
    #vi kommer flyttas ur vår planerade path, hur gör vi då?
    #lägger vi till den nya tile i vår path? eller höjer cost?
    #en tanke är att man gör en array med cost steg för steg
    #så kan man lägga på mycket cost för att gå in i en ström som drar
    #åt ett fel håll

    #Vi kan behöva titta på tile live om de andra spelarna kan vända strömmarna
    #och få oss att krascha, likaså kan vi använda det mot dem >:)
    elif((stream_direction == "n" and move_direction == "w") or
         (stream_direction == "n" and move_direction == "e") or
         (stream_direction == "e" and move_direction == "n") or
         (stream_direction == "e" and move_direction == "s") or
         (stream_direction == "s" and move_direction == "e") or
         (stream_direction == "s" and move_direction == "w") or
         (stream_direction == "w" and move_direction == "n") or
         (stream_direction == "w" and move_direction == "s")):
        print("uh oh...")
        # cost = cost + 100
        return 0

def elevation_count(move_direction, elevation_direction, elevation):
    #nerförsbacke
    if((elevation_direction == "n" and move_direction == "s") or
       (elevation_direction == "s" and move_direction == "n") or
       (elevation_direction == "e" and move_direction == "w") or
       (elevation_direction == "w" and move_direction == "e")):
        return - elevation

    #uppförsbacke
    elif elevation_direction == move_direction:
        return elevation



    #Nedan är bugg...
    #vi kommer flyttas ur vår planerade path, hur gör vi då?
    #lägger vi till den nya tile i vår path? eller höjer cost?
    #en tanke är att man gör en array med cost steg för steg
    #så kan man lägga på mycket cost för att gå in i en ström som drar
    #åt ett fel håll

    #Vi kan behöva titta på tile live om de andra spelarna kan vända strömmarna
    #och få oss att krascha, likaså kan vi använda det mot dem >:)
    elif((elevation_direction == "n" and move_direction == "w") or
         (elevation_direction == "n" and move_direction == "e") or
         (elevation_direction == "e" and move_direction == "n") or
         (elevation_direction == "e" and move_direction == "s") or
         (elevation_direction == "s" and move_direction == "e") or
         (elevation_direction == "s" and move_direction == "w") or
         (elevation_direction == "w" and move_direction == "n") or
         (elevation_direction == "w" and move_direction == "s")):
        print("uh oh...")
        # cost = cost + 100
        return 0

def test(game_id, state, tiles, path):
    path.pop(0)
    win_tile = path[-1]
    # current_tile = path.pop(0)
    sub_list = []
    cost_path = []
    cost_move_direction = []
    processed_path = []
    target_tiles = []
    while path:
        player = state["yourPlayer"]
        stamina = player["stamina"]
        # POWERUPPS
        # powerupp(game_id, player["powerupInventory"])
        inventory = player["powerupInventory"]
        if inventory:
            response = _api.use_powerup(game_id, inventory[0])
            state = response["gameState"]
            continue

        print("turn: " + str(state["turn"]))

        if stamina < 30:
            print("Vilade en omgång, stamina: " + str(stamina))
            response = _api.rest(game_id)
            state = response["gameState"]
            continue
        if len(path) >= 7:

            path_range = 7
        else:
            path_range = len(path)

        for index in range(path_range):
            sub_list.append(path[index])

        now = (player["yPos"], player["xPos"])
        straight_count = 0
        while sub_list:
            next_tile = sub_list.pop(0)
            if((next_tile[1] == now[1]) or next_tile[0] == now[0]):
                straight_count = straight_count + 1
            else:
                break

        # print("Can move " + str(straight_count) + " straight")

        straight_count = straight_count

        move_direction = ""
        now = (player["yPos"], player["xPos"])
        last_tile = path[straight_count - 1]
        if((last_tile[0] > now[0]) and last_tile[1] == now[1]):
            move_direction = "s"
        if((last_tile[0] < now[0]) and last_tile[1] == now[1]):
            move_direction = "n"
        if((last_tile[0] == now[0]) and last_tile[1] > now[1]):
            move_direction = "e"
        if((last_tile[0] == now[0]) and last_tile[1] < now[1]):
            move_direction = "w"
        if move_direction == "":
            if last_tile == now:
                return state["turn"]
            print("något gick snett med movedirection")
            print("last_tile: " + str(last_tile))
            print("Now:       " + str(now))


        for index in range(straight_count):
            sub_list.append(path[index])
        cost = 0
        while sub_list:
            now = sub_list.pop(0)
            tile = tiles[now[0]][now[1]]
            if tile["type"] == "start":
                cost = cost + 0
            elif tile["type"] == "grass":
                cost = cost + TILE_POINT_GRASS
                if "elevation" in tile:
                    elevation_direction = tile["elevation"]["direction"]
                    elevation = tile["elevation"]["amount"]
                    cost = cost + elevation_count(move_direction, elevation_direction, elevation)
            elif tile["type"] == "trail":
                cost = cost + TILE_POINT_TRAIL
                if "elevation" in tile:
                    elevation_direction = tile["elevation"]["direction"]
                    elevation = tile["elevation"]["amount"]
                    # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                    cost = cost + elevation_count(move_direction, elevation_direction, elevation)

            elif tile["type"] == "road":
                cost = cost + TILE_POINT_ROAD
                if "elevation" in tile:
                    elevation_direction = tile["elevation"]["direction"]
                    elevation = tile["elevation"]["amount"]
                    # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                    cost = cost + elevation_count(move_direction, elevation_direction, elevation)

            elif tile["type"] == "water":
                cost = cost + TILE_POINT_WATER
                if "waterstream" in tile:
                    stream_direction = tile["waterstream"]["direction"]
                    stream_speed = tile["waterstream"]["speed"]

                    # cost = cost + stream_calc(move_direction, stream_direction, stream_speed)
                    cost = cost + stream_calc(move_direction, stream_direction, stream_speed)
            elif tile["type"] == "win":
                cost = cost + TILE_POINT_START_WIN
                if "elevation" in tile:
                    elevation_direction = tile["elevation"]["direction"]
                    elevation = tile["elevation"]["amount"]
                    # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                    cost = cost + elevation_count(move_direction, elevation_direction, elevation)
            else:
                break

        print("stamina: " + str(stamina))
        if stamina < 60:
            speed = "slow"

        else:
            if cost >= MOVEMENT_POINT_SLOW:
                speed = "slow"

            if cost >= MOVEMENT_POINT_MEDIUM:
                speed = "medium"

            if cost >= MOVEMENT_POINT_FAST and stamina > 60:
                speed = "fast"



        if speed == "slow":

            if (MOVEMENT_POINT_SLOW - cost) > TILE_POINT_TREE:
                sub_list = []
                for index in range(straight_count):
                    sub_list.append(path[index])
                movepoints = MOVEMENT_POINT_SLOW
                while movepoints >= 0 and sub_list:
                    end_pos = sub_list.pop(0)
                    tile = tiles[end_pos[0]][end_pos[1]]
                    if tile["type"] == "grass":
                        movepoints = movepoints - TILE_POINT_GRASS
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "road":
                        movepoints = movepoints - TILE_POINT_ROAD
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "trail":
                        movepoints = movepoints - TILE_POINT_TRAIL
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "win":
                        movepoints = movepoints - TILE_POINT_START_WIN
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "water":
                        movepoints = movepoints - TILE_POINT_WATER
                        if "waterstream" in tile:
                            stream_direction = tile["waterstream"]["direction"]
                            stream_speed = tile["waterstream"]["speed"]
                            movepoints = movepoints - stream_calc(move_direction, stream_direction, stream_speed)

                end_tile = tiles[end_pos[0]][end_pos[1]]

                if end_tile["type"] == "forest"  or tile["type"] == "rockywater" or end_pos not in path:
                    speed = "step"
        if speed == "medium":

            if (MOVEMENT_POINT_MEDIUM - cost) > TILE_POINT_TREE and sub_list:
                sub_list = []
                for index in range(straight_count):
                    sub_list.append(path[index])
                movepoints = MOVEMENT_POINT_MEDIUM
                while movepoints >= 0:
                    end_pos = sub_list.pop(0)
                    tile = tiles[end_pos[0]][end_pos[1]]
                    if tile["type"] == "grass":
                        movepoints = movepoints - TILE_POINT_GRASS
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "road":
                        movepoints = movepoints - TILE_POINT_ROAD
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "trail":
                        movepoints = movepoints - TILE_POINT_TRAIL
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "win":
                        movepoints = movepoints - TILE_POINT_START_WIN
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "water":
                        movepoints = movepoints - TILE_POINT_WATER
                        if "waterstream" in tile:
                            stream_direction = tile["waterstream"]["direction"]
                            stream_speed = tile["waterstream"]["speed"]
                            movepoints = movepoints - stream_calc(move_direction, stream_direction, stream_speed)
                end_tile = tiles[end_pos[0]][end_pos[1]]

                if end_tile["type"] == "forest"  or tile["type"] == "rockywater" or end_pos not in path:
                    speed = "slow"

        if speed == "fast":

            if (MOVEMENT_POINT_FAST - cost) > TILE_POINT_TREE and sub_list:
                sub_list = []
                for index in range(straight_count):
                    sub_list.append(path[index])
                movepoints = MOVEMENT_POINT_FAST
                while movepoints >= 0:
                    end_pos = sub_list.pop(0)
                    tile = tiles[end_pos[0]][end_pos[1]]
                    if tile["type"] == "grass":
                        movepoints = movepoints - TILE_POINT_GRASS
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "road":
                        movepoints = movepoints - TILE_POINT_ROAD
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "trail":
                        movepoints = movepoints - TILE_POINT_TRAIL
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "win":
                        movepoints = movepoints - TILE_POINT_START_WIN
                        if "elevation" in tile:
                            elevation_direction = tile["elevation"]["direction"]
                            elevation = tile["elevation"]["amount"]
                            movepoints = movepoints - elevation_count(move_direction, elevation_direction, elevation)

                    elif tile["type"] == "water":
                        movepoints = movepoints - TILE_POINT_WATER
                        if "waterstream" in tile:
                            stream_direction = tile["waterstream"]["direction"]
                            stream_speed = tile["waterstream"]["speed"]
                            movepoints = movepoints - stream_calc(move_direction, stream_direction, stream_speed)
                end_tile = tiles[end_pos[0]][end_pos[1]]

                if end_tile["type"] == "forest"  or tile["type"] == "rockywater" or end_pos not in path:
                    speed = "medium"


        if path_range == 1 or speed == "step":
            speed = "step"
            response = _api.step(game_id, move_direction)
        else:
            response = _api.make_move(game_id, move_direction, speed)


        state = response["gameState"]
        player = state["yourPlayer"]
        player_pos = (player["yPos"], player["xPos"])
        if player_pos in path:
            i = path[0]
            while i != player_pos:
                i = path.pop(0)
        else:
            best = 100000
            best_i = 0

            range_index = 10
            if len(path) < 10:
                range_index = len(path)
            for i in range(range_index):
                test_tile = path[i]
                distance = math.sqrt((test_tile[0] - player["yPos"])**2 + (test_tile[1] - player["xPos"])**2)
                # distance = (abs(player["yPos"] - test_tile[0]) +
                #             abs(player["xPos"] - test_tile[1]))
                if distance <= best:
                    best = distance
                    best_i = i

            print("best_i: " + str(best_i) + " distance: " + str(distance))
            #while next_tile != target_tiles[0]:
            if best_i > 0:
                for i in range(best_i):
                    next_tile = path.pop(0)
            else:
                next_tile = path.pop(0)
            check = 0
            while (next_tile[0] != player["yPos"] or next_tile[1] != player["xPos"]):
                if check > 15:
                    return

                check = check + 1
                print("Correction: " + str(state["turn"]))
                print("target y: " + str(next_tile[0]) + " target x: " + str(next_tile[1]))
                print("player y: " + str(player["yPos"]) + " player x: " + str(player["xPos"]))
                state = response["gameState"]
                player = state["yourPlayer"]
                player_pos = (player["yPos"], player["xPos"])
                if player_pos in path:
                    i = path[0]
                    while i != player_pos:
                        i = path.pop(0)
                    break
                print(next_tile)
                if next_tile[0] > player["yPos"]:
                    tile = tiles[player_pos[0] + 1][player_pos[1]]
                    print(tile)
                    if tile["type"] != "forest" and tile["type"] != "rockywater":
                        response = _api.step(game_id, "s")
                        state = response["gameState"]
                        player = state["yourPlayer"]
                        continue
                if next_tile[0] < player["yPos"]:
                    tile = tiles[player_pos[0] - 1][player_pos[1]]
                    print(tile)
                    if tile["type"] != "forest" and tile["type"] != "rockywater":
                        response = _api.step(game_id, "n")
                        state = response["gameState"]
                        player = state["yourPlayer"]
                        continue
                if next_tile[1] < player["xPos"]:
                    tile = tiles[player_pos[0]][player_pos[1] - 1]
                    print(tile)
                    if tile["type"] != "forest" and tile["type"] != "rockywater":
                        response = _api.step(game_id, "w")
                        state = response["gameState"]
                        player = state["yourPlayer"]
                        continue
                if next_tile[1] > player["xPos"]:
                    tile = tiles[player_pos[0]][player_pos[1] + 1]
                    print(tile)
                    if tile["type"] != "forest" and tile["type"] != "rockywater":
                        response = _api.step(game_id, "e")
                        state = response["gameState"]
                        player = state["yourPlayer"]
                        continue

            # if win_tile == (player["yPos"], player["xPos"]):






    # target_copy = copy.deepcopy(target_tiles)
    # draw_map(state, target_copy)
    print("Imål! Omgång nr: " + str(state["turn"]))
    return state["turn"]


# TODO: POWERUPPS
def powerupp(game_id, inventory):
    """Här ska all powerupp ske"""
    if inventory:
        _api.use_powerup(game_id, inventory[0])


# Går den förutbestämda banan:
def walk_path(game_id, state, path):
    """Försöker gå vägen som man skickar med i path"""
    while path:
        # tiles = state["tileInfo"]
        player = state["yourPlayer"]
        next_tile = path.pop(0)

        # POWERUPPS
        powerupp(game_id, player["powerupInventory"])
        movespeed = "slow"
        if next_tile[0] == player["yPos"] and next_tile[1] == player["xPos"]:
            print("Allready on tile")
            continue
        while (next_tile[0] != player["yPos"] or next_tile[1] != player["xPos"]):
            response = ""
            if player["stamina"] > 65:
                movespeed = "medium"
            else:
                movespeed = "slow"
            if next_tile[0] != player["yPos"] and next_tile[0] > player["yPos"]:
                # Flytta söder
                # response = _api.make_move(game_id, "s", speed=movespeed)
                response = _api.step(game_id, "s")
            elif next_tile[0] != player["yPos"] and next_tile[0] < player["yPos"]:
                # Flytta norr
                # response = _api.make_move(game_id, "n", speed=movespeed)
                response = _api.step(game_id, "n")
            elif next_tile[1] < player["xPos"]:
                # Flytta väster
                # response = _api.make_move(game_id, "w", speed=movespeed)
                response = _api.step(game_id, "w")
            elif next_tile[1] > player["xPos"]:
                # Flytta öster
                # response = _api.make_move(game_id, "e", speed=movespeed)
                response = _api.step(game_id, "e")
            print("Turn: " + str(state["turn"]))
            state = response["gameState"]
            # tiles = state["tileInfo"]
            player = state["yourPlayer"]
            # player_pos = (player["yPos"], player["xPos"])




def solution(game_id):
    """Lösningen ;)"""
    # Öppnar en tab i webbläsaren för att se gubben springa:
    # Ladda om sidan efter en stund så fungerar det!
    website = "http://www.theconsidition.se/ironmanvisualizer?gameId="
    website = website + str(game_id)
    webbrowser.open_new_tab(website)

    initial_state = _api.get_game(game_id)
    if initial_state["success"] is True:
        state = initial_state["gameState"]
        tiles = state["tileInfo"]

        # player = state["yourPlayer"]
        start = (0, 0)
        end = (0, 0)

        # Hitta start och slutnod:
        start_and_finish = find_start_and_finish(tiles)
        for i in start_and_finish:
            if i["type"] == "start":
                start = (i["cordy"], i["cordx"])
            if i["type"] == "win":
                end = (i["cordy"], i["cordx"])
        print("Startnode: " + str(start))
        print("Endnode: " + str(end))

        # !!!!!!!!!!!!TEST!!!!!!!!!!!!!!!!!!
        # end = (41, 13)
        # Beräknar närmaste vägen till mål:
        path = astar(tiles, start, end)
        print("Tiles to finish: " + str(len(path)))
        print(path[100])
        path_copy = path
        # cost_path = speed_planner(game_id, state, tiles, path_copy)
        # print(cost_path)
        return test(game_id, state, tiles, path_copy)
        # Kör denna rad om du vill ha karta i konsolen. OBS! Då fungerar inte
        # körningen mot apit:
        # TODO
        # draw_map(state, path)

        # Kör banan:
        #walk_path(game_id, state, path)

        print("Game-id: " + str(game_id))


def main():
    """main"""
    game_id = ""
    #If no gameID is specified as parameter to the script,
    #Initiate a game with 1 player on the standard map
    if len(sys.argv) == 1:
        #Can only have 2 active games at once. This will end any previous ones:
        best_score = 1000
        ok = False
        if ok:
            for i in range(3):
                _api.end_previous_games_if_any()
                game_id = _api.init_game()
                joined_game = _api.join_game(game_id)
                readied_game = _api.try_ready_for_game(game_id)
                if readied_game:
                    print("Joined and readied! Solving...")
                    t = solution(game_id)
                    if t < best_score:
                        best_score = t

            print(best_score)
        else:
            _api.end_previous_games_if_any()
            game_id = _api.init_game()
            joined_game = _api.join_game(game_id)
            readied_game = _api.try_ready_for_game(game_id)
            if readied_game:
                print("Joined and readied! Solving...")
                t = solution(game_id)


    else:
        game_id = sys.argv[1]
        joined_game = _api.join_game(game_id)
        readied_game = _api.try_ready_for_game(game_id)
        if readied_game:
            print("Joined and readied! Solving...")
            solution(game_id)


main()
