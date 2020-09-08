#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 20 18:11:23 2020

@author: lorenzo
"""

import json
import numpy as np
import matplotlib.pyplot as plt

from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from pymatgen.analysis.phase_diagram import PDPlotter

from my_functions.phase_diagram.analysis import Chempots, ChempotAnalysis, PDHandler, PDPlotterAdder

print('\nReading chemical potentials data...\n')

with open('chempots_boundary.json') as file:
    chempots_boundary = json.load(file)   
with open('chempots_experimental.json') as file:
    chempots_exp = json.load(file)
    
print('Reading phase diagram entries from file...\n')

with open('PD_Na-Nb-O_NN_cubic.json') as json_file:
	computed_phases = json.load(json_file)

ca = ChempotAnalysis(computed_phases)
# building from dict
chempots_boundary = {res : Chempots.from_dict(chempots_boundary[res]).chempots for res in chempots_boundary}
chempots_exp = {res : Chempots.from_dict(chempots_exp[res]).chempots for res in chempots_exp}
# getting delta chempots
chempots_boundary = {res : ca.get_chempots_delta(chempots_boundary[res]) for res in chempots_boundary}
chempots_exp = {res : ca.get_chempots_delta(chempots_exp[res]) for res in chempots_exp}

    
pd = PDHandler(computed_phases).phase_diagram()
pdplotter = PDPlotter(pd)

#building set of points
points_boundary = {}
for res in chempots_boundary:
    points_boundary[res] = (chempots_boundary[res][Element('Na')],chempots_boundary[res][Element('Nb')])
    
points_exp = {}
for res in chempots_exp:
    points_exp[res] = (chempots_exp[res][Element('Na')],chempots_exp[res][Element('Nb')])

pdplotter.get_chempot_range_map_plot([Element('Na'),Element('Nb')])
plotcustom = PDPlotterAdder(ca)
plotcustom.add_points(points_boundary)
plotcustom.add_points(points_exp)

chempot_ox = {Element('O'):chempots_exp['X1'][Element('O')]}
              
plotcustom.add_constant_chempot_line(Composition('NaNbO3'), Element('Na'), chempot_ox,
                                     label='$\Delta \mu_{O}$ = '+ '%s eV'%(np.around(chempot_ox[Element("O")],decimals=2)),color='#5782e1' 
                                     )
plt.legend() 
plt.xlim(-3.5,0.4)
plt.ylim(-11.5,1)  

plt.savefig('stability_diagram_complete.pdf')    
    

