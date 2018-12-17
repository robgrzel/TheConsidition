from api import API
import random
import sys

# Insert your API-key here
_api_key = "e7645575-8726-4abf-b75f-a1fe53604ce3"
#Specify your API-key number of players per game),
# mapname, and number of waterstreams/elevations/powerups here
_api = API(_api_key, 1, "standardmap", 10, 10, 10)

def findStartAndFinish(tiles):
    startAndFinish = []
    for y in range(100):
        for x in range(100):
            tile = tiles[y][x]
            if tile["type"] != "forest" and tile["type"] != "road" and tile["type"] != "water" and tile["type"] != "trail" and tile["type"] != "grass":
                out = str(tile["type"])+ " cord: " + str(y)+ ":" + str(x)
                tile["cordx"] = x
                tile["cordy"] = y
                print(out)
                startAndFinish.append(tile)
    return startAndFinish

#A "solution" that takes a step in a random direction every turn
def solution(game_id):
    initial_state = _api.get_game(game_id)
    if (initial_state["success"] == True):
        state = initial_state["gameState"]
        tiles = state["tileInfo"]
        current_player = state["yourPlayer"]
        current_y_pos = current_player["yPos"]
        current_x_pos = current_player["xPos"]
        startAndFinish = findStartAndFinish(tiles)
        print(startAndFinish)
        for i in range(5):
            print(tiles[90 + i][44])
        for i in range(5):
            print(tiles[92][42 + i])
        # print(tiles)
    else:
        print(initial_state["message"])


def main():
    game_id = ""
    #If no gameID is specified as parameter to the script,
    #Initiate a game with 1 player on the standard map
    if (len(sys.argv) == 1):
        _api.end_previous_games_if_any() #Can only have 2 active games at once. This will end any previous ones.
        game_id = _api.init_game()
        joined_game = _api.join_game(game_id)
        readied_game = _api.try_ready_for_game(game_id)
        if (readied_game != None):
            print("Joined and readied! Solving...")
            solution(game_id)
    else:
        game_id = sys.argv[1]

main()
