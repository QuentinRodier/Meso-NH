# -*- coding: utf-8 -*-

__author__ = "Aurélien Costes"
__copyright__ = "copyright 2019, Météo-France Cerfacs"
__credits__ = ["Aurélien Costes"]
__license__ = "CeCILL-C"
__version__ = "0.0.1"
__maintainer__ = "Aurélien Costes"
__email__ = "aurelien.costes@meteo.fr"
__status__ = "Development"

import sys, os
import argparse
import numpy as np
import matplotlib.pyplot as plt
from math import radians, sqrt, pow, ceil, exp, tan
from math import atan, degrees
import csv

# parameters
showfig = False
Savecsv = True

Nx = 200
Ny = 30

dx = 25
dy = 25

minheight = 0
maxheight = 450

# Break slopePoints
# upslope start, downslope start
Offset = [500, 2500]

minzeroslopeend = 500

# up slope angle [deg]
Upslopeangle = 30
# down slope angle [deg]
Downslopeangle = 30

x = np.linspace(dx / 2, Nx * dx - dx / 2, Nx)
y = np.linspace(dy / 2, Ny * dy - dy / 2, Ny)

X, Y = np.meshgrid(x, y)

hmap = np.zeros((Ny, Nx))

upsloperad = radians(Upslopeangle)
downsloperad = radians(Downslopeangle)

z = minheight
for i in range(Nx):
    if x[i] <= Offset[0]:
        hmap[:, i] = z
        continue
    if x[i] >= np.max(x) - minzeroslopeend:
        hmap[:, i] = z
        continue
    # downslope
    if x[i] >= min(Offset[1], np.max(x) - minzeroslopeend - z / tan(downsloperad)):
        z = max(minheight, z - tan(downsloperad) * dx)
        hmap[:, i] = z
        continue
    # upslope
    z = min(maxheight, z + tan(upsloperad) * dx)
    hmap[:, i] = z


if showfig:
    fig = plt.figure()
    plt.plot(x, hmap[0, :], ".-", markersize=3)
    plt.show()


if Savecsv:
    with open("CstSlope.csv", "w") as f:
        writer = csv.writer(f)
        for j in range(Ny):
            row = hmap[j, :]
            writer.writerow(row)
