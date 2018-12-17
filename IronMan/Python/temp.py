def speed_planner(game_id, state, tiles, path):
    path.pop(0)
    stamina = 100
    # current_tile = path.pop(0)
    sub_list = []
    cost_path = []
    cost_move_direction = []
    processed_path = []
    target_tiles = []
    while path:
        if stamina > 100:
            stamina = 100
        if len(path) >= 7:

            path_range = 7
        else:
            path_range = len(path)

        for index in range(path_range):
            sub_list.append(path[index])

        now = sub_list.pop(0)
        straight_count = 0
        while sub_list:
            next_tile = sub_list.pop(0)
            if((next_tile[1] == now[1]) or next_tile[0] == now[0]):
                straight_count = straight_count + 1
            else:
                break

        print("Can move " + str(straight_count) + " straight")


        for index in range(straight_count):
            sub_list.append(path[index])

        move_direction = ""
        now = path[0]
        last_tile = path[straight_count]
        if path_range == 1:
            now = processed_path[len(processed_path) - 1]
        print("Now: " + str(now) +  " last_tile: " + str(last_tile))
        if((last_tile[0] > now[0]) and last_tile[1] == now[1]):
            move_direction = "s"
            print("Moving s")
        if((last_tile[0] < now[0]) and last_tile[1] == now[1]):
            move_direction = "n"
            print("Moving n")
        if((last_tile[0] == now[0]) and last_tile[1] > now[1]):
            move_direction = "e"
            print("Moving e")
        if((last_tile[0] == now[0]) and last_tile[1] < now[1]):
            move_direction = "w"
            print("Moving w")

        cost = 0
        elevation_and_water = 0
        while sub_list:
            now = sub_list.pop(0)
            tile = tiles[now[0]][now[1]]
            if tile["type"] == "start":
                cost = cost + 0
            elif tile["type"] == "grass":
                cost = cost + TILE_POINT_GRASS
            elif tile["type"] == "trail":
                cost = cost + TILE_POINT_TRAIL

            elif tile["type"] == "road":
                cost = cost + TILE_POINT_ROAD

            elif tile["type"] == "water":
                cost = cost + TILE_POINT_WATER
            elif tile["type"] == "win":
                cost = cost + TILE_POINT_START_WIN
            else:
                break

        print("Cost: " + str(cost))
        if stamina < 60:
            speed = "slow"
            stamina = stamina - 10

        else:
            if cost >= MOVEMENT_POINT_SLOW:
                speed = "slow"
                stamina = stamina - 10

            if cost >= MOVEMENT_POINT_MEDIUM:
                speed = "medium"
                stamina = stamina - 30

            if cost >= MOVEMENT_POINT_FAST:
                speed = "fast"
                stamina = stamina - 50



        for index in range(straight_count):
            sub_list.append(path[index])

        tiles_in_path = []
        while sub_list:
            now = sub_list.pop(0)
            tile = tiles[now[0]][now[1]]
            if tile["type"] == "start":
                cost = cost + 0
                tiles_in_path.append("start")
            elif tile["type"] == "grass":
                tiles_in_path.append("grass")

            elif tile["type"] == "trail":
                tiles_in_path.append("trail")

            elif tile["type"] == "road":
                tiles_in_path.append("road")

            elif tile["type"] == "water":
                tiles_in_path.append("water")

            elif tile["type"] == "win":
                tiles_in_path.append("win")


        if move_direction == "":
            print("något gick snett med movedirection")

        if speed == "slow":
            if (MOVEMENT_POINT_SLOW - cost) > TILE_POINT_TREE:
                now = path[0]
                if move_direction == "n":
                    tile = tiles[(now[0] - 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "step"
                elif move_direction == "s":
                    tile = tiles[(now[0] + 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "step"
                elif move_direction == "w":
                    tile = tiles[now[0]][(now[1] - 1)]
                    if tile["type"] == "forest":
                        speed = "step"
                elif move_direction == "e":
                    tile = tiles[now[0]][(now[1] + 1)]
                    if tile["type"] == "forest":
                        speed = "step"

        elif speed == "medium":
            if (MOVEMENT_POINT_MEDIUM - cost) > TILE_POINT_TREE:
                now = path[0]
                if move_direction == "n":
                    tile = tiles[(now[0] - 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "slow"
                elif move_direction == "s":
                    tile = tiles[(now[0] + 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "slow"
                elif move_direction == "w":
                    tile = tiles[now[0]][(now[1] - 1)]
                    if tile["type"] == "forest":
                        speed = "slow"
                elif move_direction == "e":
                    tile = tiles[now[0]][(now[1] + 1)]
                    if tile["type"] == "forest":
                        speed = "slow"

        elif speed == "fast":
            if (MOVEMENT_POINT_FAST - cost) > TILE_POINT_TREE:
                now = path[0]
                if move_direction == "n":
                    tile = tiles[(now[0] - 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "medium"
                elif move_direction == "s":
                    tile = tiles[(now[0] + 1)][now[1]]
                    if tile["type"] == "forest":
                        speed = "medium"
                elif move_direction == "w":
                    tile = tiles[now[0]][(now[1] - 1)]
                    if tile["type"] == "forest":
                        speed = "medium"
                elif move_direction == "e":
                    tile = tiles[now[0]][(now[1] + 1)]
                    if tile["type"] == "forest":
                        speed = "medium"

        if path_range == 1 or speed == "step":
            speed = "step"
            target = path[0]
            processed_path.append(path.pop(0))

        else:
            sub_list = []
            for index in range(straight_count):
                sub_list.append(path[index])

            if speed == "fast":
                movepoints = MOVEMENT_POINT_FAST
            elif speed == "medium":
                movepoints = MOVEMENT_POINT_MEDIUM
            elif speed == "slow":
                movepoints = MOVEMENT_POINT_SLOW

            tiles_in_path = []
            # if elevation_and_water >= 0:
            #     movepoints = movepoints - elevation_and_water
            # else:
            #     movepoints = movepoints + abs(elevation_and_water)
            while (movepoints > TILE_POINT_START_WIN) and sub_list:
                target = path[0]
                processed_path.append(path.pop(0))
                now = sub_list.pop(0)
                tile = tiles[now[0]][now[1]]
                if tile["type"] == "start":
                    movepoints = movepoints - TILE_POINT_START_WIN

                elif tile["type"] == "grass":
                    if "elevation" in tile:
                        elevation_direction = tile["elevation"]["direction"]
                        elevation = tile["elevation"]["amount"]
                        # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                        movepoints = movepoints + elevation_count(move_direction, elevation_direction, elevation)
                    movepoints = movepoints - TILE_POINT_GRASS
                elif tile["type"] == "trail":
                    if "elevation" in tile:
                        elevation_direction = tile["elevation"]["direction"]
                        elevation = tile["elevation"]["amount"]
                        # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                        movepoints = movepoints + elevation_count(move_direction, elevation_direction, elevation)
                    movepoints = movepoints - TILE_POINT_TRAIL

                elif tile["type"] == "road":
                    if "elevation" in tile:
                        elevation_direction = tile["elevation"]["direction"]
                        elevation = tile["elevation"]["amount"]
                        # cost = cost + elevation_count(move_direction, elevation_direction, elevation)
                        movepoints = movepoints + elevation_count(move_direction, elevation_direction, elevation)

                    movepoints = movepoints - TILE_POINT_ROAD

                elif tile["type"] == "water":
                    if "waterstream" in tile:
                        stream_direction = tile["waterstream"]["direction"]
                        stream_speed = tile["waterstream"]["speed"]

                        # cost = cost + stream_calc(move_direction, stream_direction, stream_speed)
                        movepoints = movepoints + stream_calc(move_direction, stream_direction, stream_speed)
                    movepoints = movepoints - TILE_POINT_WATER

                elif tile["type"] == "win":
                    movepoints = movepoints - TILE_POINT_START_WIN

            sub_list = []

        print(str(stamina))
        if stamina < 60:
            stamina = stamina + 15
        else:
            stamina = stamina + 20
        target_tiles.append(target)
        cost_path.append(speed)
        cost_move_direction.append(move_direction)

    target_copy = copy.deepcopy(target_tiles)
    draw_map(state, target_copy)
    print(cost_move_direction)

    while cost_path:

        next_speed = cost_path.pop(0)
        next_move = cost_move_direction.pop(0)

        # POWERUPPS
        # player = state["yourPlayer"]
        # powerupp(game_id, player["powerupInventory"])
        if next_speed == "step":
            response = _api.step(game_id, next_move)
        else:
            response = _api.make_move(game_id, next_move, next_speed)

        state = response["gameState"]
        player = state["yourPlayer"]
        #next_tile = target_tiles.pop(0)
        #hitta närmaste target tile
        best = 100000
        best_i = 0

        range_index = 10
        if len(target_tiles) < 10:
            range_index = len(target_tiles)
        for i in range(range_index):
            test_tile = target_tiles[i]
            distance = math.sqrt((test_tile[0] - player["yPos"])**2 + (test_tile[1] - player["xPos"])**2)
            if distance <= best:
                best = distance
                best_i = i
                # next_tile = copy.deepcopy(test_tile)

        print("best_i: " + str(best_i) + " distance: " + str(distance))
        #while next_tile != target_tiles[0]:
        for i in range(best_i):
            next_tile = target_tiles.pop(0)
        next_tile = target_tiles.pop(0)
        tiles = state["tileInfo"]
        while (next_tile[0] != player["yPos"] or next_tile[1] != player["xPos"]):
            print("Correction: " + str(state["turn"]))
            print("target y: " + str(next_tile[0]) + " target x: " + str(next_tile[1]))
            print("player y: " + str(player["yPos"]) + " player: x" + str(player["xPos"]))
            state = response["gameState"]
            player = state["yourPlayer"]
            player_pos = (player["yPos"], player["xPos"])
            if next_tile[0] > player["yPos"]:
                tile = tiles[player_pos[0] + 1][player_pos[1]]
                print(tile)
                if tile["type"] != "forest":
                    response = _api.step(game_id, "s")
                    state = response["gameState"]
                    player = state["yourPlayer"]
                    continue
            if next_tile[0] < player["yPos"]:
                tile = tiles[player_pos[0] - 1][player_pos[1]]
                print(tile)
                if tile["type"] != "forest":
                    response = _api.step(game_id, "n")
                    state = response["gameState"]
                    player = state["yourPlayer"]
                    continue
            if next_tile[1] < player["xPos"]:
                tile = tiles[player_pos[0]][player_pos[1] - 1]
                print(tile)
                if tile["type"] != "forest":
                    response = _api.step(game_id, "w")
                    state = response["gameState"]
                    player = state["yourPlayer"]
                    continue
            if next_tile[1] > player["xPos"]:
                tile = tiles[player_pos[0]][player_pos[1] + 1]
                print(tile)
                if tile["type"] != "forest":
                    response = _api.step(game_id, "e")
                    state = response["gameState"]
                    player = state["yourPlayer"]
                    continue
            return



    return cost_path
