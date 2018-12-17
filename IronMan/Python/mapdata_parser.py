direction_types = {
    'w': 400,
    's': 500,
    'e': 600,
    'n': 700,
    'aoe': 800
}

point_types = {
    'nowaypoint' : 0,
    'waypoint' : 1,
    'midpoint': 2,
}

terrain_types = {
    'road': 1000,
     'trail': 1100,
     'grass': 1200,
     'water': 1300,
     'rockywater': 1400,
     'forest': 1500,
     'start': 1600,
     'win': 1700
                 }

terrain_dynamics = {'rain': 10000,
                    'elevation': 20000,
                    'waterstream': 30000,
                    'powerup': 40000}

powerup_types = dict(
    RemoveCloud=1010015,
    RestoreStamina=1100011,
    InvertStreams=1030012,
    Shoes=2201113,
    Flippers=2201313,
    Cycletire=2201013,
    Umbrella=2110014,
    Energyboost=2100015,
    Potion=2200016,
    Helmet=2300017,
    StaminaSale=2100018,
    Spikeshoes=2401119,
    Cyklop=2401319,
    BicycleHandlebar=2401019
)

import time
import json
import numpy as np
from pprint import pprint as ppr
import matplotlib.pyplot as plt
from skimage.morphology import skeletonize
from skimage.util import invert


def mapdata_parser(path, stateFname='state', mapdataFname='mapdata', doOnlyDyn=True, doSkeletonizeStatic=False):
    with open('%s/%s.json' % (path, stateFname), 'r') as statefile:
        state = json.load(statefile)
        tileInfo = state['tileInfo']
        mapTerrain = np.zeros([100, 100])
        mapDynamics = np.zeros([100, 100])

        for i, row in enumerate(tileInfo):
            for j, col in enumerate(row):
                terrainType = terrain_types[col.pop('type')]
                mapTerrain[i, j] = terrainType
                if len(col):
                    for k in col.keys():
                        v = 0
                        if k == 'elevation':
                            v = col[k]
                            amount = v['amount']
                            direction = direction_types[v['direction']]
                            dynamics = terrain_dynamics[k]
                            v = dynamics + direction + amount
                        elif k == 'waterstream':
                            v = col[k]
                            speed = v['speed']
                            direction = direction_types[v['direction']]
                            dynamics = terrain_dynamics[k]
                            v = dynamics + direction + speed
                        elif k == 'weather':
                            v = terrain_dynamics[col[k]]
                        elif k == 'powerup':
                            v = powerup_types[col[k]['name']]
                        mapDynamics[i, j] = v

    if doSkeletonizeStatic:
        minimap_parser(mapTerrain)

    if doOnlyDyn:
        with open('%s/%s.dynamic.csv' % (path, mapdataFname), 'w') as mapdatafile:
            for i in range(100):
                for j in range(100):
                    if mapDynamics[i, j]:
                        mapdatafile.write("%d,%d,%d\n" % (i, j, mapDynamics[i, j]))
        return mapDynamics

    if not doOnlyDyn:

        with open('%s/%s.static.csv' % (path, mapdataFname), 'w') as mapdatafile:
            for i in range(100):
                for j in range(100):
                    mapdatafile.write("%d\n" % (mapTerrain[i, j]))
        return mapDynamics, mapTerrain


def minimap_parser(mapdataOrginal):
    mapdata = mapdataOrginal.copy()
    for k in terrain_types:
        waypoint = (k == 'road') | \
                   (k == 'trail') | \
                   (k == 'grass') | \
                   (k == 'water') | \
                   (k == 'win') | \
                   (k == 'start')

        tt = terrain_types[k]
        mapdata[mapdata == tt] = waypoint

        if waypoint:
            mapdataOrginal[mapdata == tt] += point_types['waypoint']

    mapdata.shape = [100,100]

    skeleton = invert(skeletonize(mapdata))*1

    mapdataOrginal[skeleton==0] += point_types['midpoint']


if __name__ == '__main__':
    try:
        mapdata_parser(path='../datadir', stateFname='state', mapdataFname='mapdata')
    except: pass