from IPython.core.display import display, HTML
from IPython.display import IFrame

def show_online_game(gameIdFile,w=1400, h=800):
    display(HTML("<style>.container { width:100% !important; }</style>"))
    try:
        gameIdFile = open(gameIdFile,'r')
        gameId = gameIdFile.read()
        gameIdFile.close()
    except:
        gameId = gameIdFile
    gamePlotHtml  = "http://www.theconsidition.se/ironmanvisualizer?gameId=%s"%gameId
    print("Start watching game online: %s"%gamePlotHtml)
    IFrame(src=gamePlotHtml, width=w, height=h)
