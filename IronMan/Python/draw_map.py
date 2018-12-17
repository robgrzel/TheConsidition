# -*- coding: utf-8 -*-
from colorama import Fore, Back, init, Style
# from tiles import all_pos
init()
import time
from pprint import pprint as ppr



def draw_map(game_state, path=False):

    if path:
        for index, tile in enumerate(path):
            path[index] = (tile[1], tile[0])
    tile_info = game_state["tileInfo"]
    for y, row in enumerate(tile_info):
        for x, col in enumerate(row):
            type = col["type"][:1]
            if "waterstream" in col:
                direction = col["waterstream"]["direction"]
            elif "elevation" in col:
                direction = col["elevation"]["direction"]
            else:
                direction = ' '
            
            if type is 'w':
                direction = direction.replace('n', '↑')\
                                    .replace('e', '→')\
                                    .replace('s', '↓')\
                                    .replace('w', '←')
            else:
                direction = direction.replace('n', '↓')\
                                     .replace('e', '←')\
                                     .replace('s', '↑')\
                                     .replace('w', '→')

            # if (x, y) in all_pos:
            #     direction = "♦"

            if (game_state["yourPlayer"]["yPos"] is y) and (game_state["yourPlayer"]["xPos"] is x):
                print("☻", end='')

            elif path and (x, y) in path:
                print(Fore.LIGHTBLACK_EX + Back.WHITE + direction, end='')

            elif type is 'f':
                print(Fore.LIGHTBLACK_EX + Back.GREEN + direction, end='')
            elif type is 'w':
                print(Fore.LIGHTBLACK_EX + Back.BLUE + direction, end='')
            elif type is 't':
                print(Fore.LIGHTBLACK_EX + Back.LIGHTMAGENTA_EX + direction, end='')
            elif type is 'g':
                print(Fore.LIGHTBLACK_EX + Back.YELLOW + direction, end='')
            elif type is "r":
                print(Fore.LIGHTBLACK_EX + Back.BLACK + direction, end='')
            elif type is "s":
                print(Fore.LIGHTBLACK_EX + Back.WHITE + "s", end='')
            else:
                print(Fore.LIGHTBLACK_EX + Back.MAGENTA + " ", end='')
        print(Style.RESET_ALL)
