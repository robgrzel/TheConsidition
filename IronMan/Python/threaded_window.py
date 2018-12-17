import tkinter as  tk
from threading import Thread as thread
import time
from draw_map import draw_map

import json
from pprint import pprint as ppr

with open('state.json', 'r') as statefile:
    state = json.load(statefile)

import os

root = tk.Tk()
termf = tk.Frame(root, height=400, width=500)

termf.pack(fill=tk.BOTH, expand=tk.YES)
wid = termf.winfo_id()
os.system('xterm -into %d -geometry 800x600 -sb &' % wid)

root.mainloop()


class T():
    def det2(self,x):
        time.sleep(2)
        x.destroy()

x = tk.Tk()
ts = thread(target=T().det2, args=(x,))
ts.daemon = True
ts.start()
x.mainloop()

