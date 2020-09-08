#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 23 11:03:36 2019

@author: villa
"""

import os
import sys
import copy
from glob import glob

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.core.composition import Composition
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter , PDEntry
from pymatgen.core.periodic_table import Element

system = ['Na','Nb','O']

system_name = '-'.join(system)


    
###############################################################################    
print('')
print('Calculating Phase Diagram...\n')

computed_phases = {}

''' Read phase diagram Entries from file'''

entries_file = open('PD_Na-Bi-Ti-O.dat','r')
lines = entries_file.readlines()
for l in range(0,len(lines)):    
    line = lines[l].split()    
    if line[0] != '#':        
        phase = line[0]
        computed_phases[phase] = float(line[1])
    
entries = []

for phase in computed_phases:
    
    # getting Composition Object
    comp = Composition(phase)
    # getting entry for PD Object
    entry = PDEntry(comp,computed_phases[phase])
    # building list of entries
    entries.append(entry)

# getting PD from list of entries
pd = PhaseDiagram(entries)


# PLOTTING
#
## get distance from convex hull for cubic phase
#comp = Composition('NaNbO3')
#energy = -38.26346361
#entry = PDEntry(comp,energy)
#cubic_instability = pd.get_e_above_hull(entry)
#
#pd_dict = pd.as_dict()
#
## Getting Plot
#plt = PDPlotter(pd, show_unstable=False)  # you can also try show_unstable=True
#
##plt_data = plt.pd_plot_data
## getting plot for chem potential - variables 'fontsize' for labels size and 'plotsize' for fig size have been added (not present in original pymatgen) to get_chempot_range_map_plot function
#chem_pot_plot = plt.get_chempot_range_map_plot([Element("Na"), Element("Nb")], 
#                                                fontsize = 14,plotsize=1.8)
#
#chem_pot_plot.savefig(f'chem_pot_{system_name}.png')  # save figure
## getting plot for PD - variables 'fontsize' for labels size and plotsize for fig size have been added (not present in original pymatgen) to get_plot function
#pd_plot = plt.get_plot(label_stable = True,fontsize = 24,plotsize = 3)
#
#pd_plot.savefig(f'PD_{system_name}.png')
#
###############################################################################
