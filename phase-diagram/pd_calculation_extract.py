#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 23 11:03:36 2019

@author: villa
"""

import os
import sys
from glob import glob

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.core.composition import Composition
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter , PDEntry
from pymatgen.core.periodic_table import Element

system = ['Na','Nb','O']

#input_dir = sys.argv[1]
input_dir = '.'


system_name = '-'.join(system)
#system_name = os.path.basename(input_dir)

input_dir = (input_dir + '/')

#opening output file
file_name = f'PD_{system_name}.dat'
file_table = open(file_name,'w') 
# system name on file
file_table.write('# System : %s \n' % (system_name))
# legend on file
file_table.write('# Phase   total_energy p.f.u.(eV) \n')                 
   
#adding wild card to input directory
wild_dir = (input_dir + '*/')
#creating a list of directories contained in input directory
list_dir = glob(wild_dir)

# initializing entries list for PD
entries = []
# initializing dict of computed data
computed_phases = {}

###############################################################################
print('')
print('Retrieving computed data...\n')
print('Phase  total energy p.f.u.\n')

for dir in list_dir:
    
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # getting name for the phase from the directory name
    phase = subfold
    
    # build path of OUTCAR file
    path_vasprun = (dir + 'vasprun.xml')
    vasprun = Vasprun(path_vasprun)
    # getting total energy PER FORMULA UNIT
    total_energy = vasprun.final_energy
    vasprun_dict = vasprun.as_dict()
    # n° atoms in calculation
    nsites = vasprun_dict['nsites']
    # dictionary for reduced cell composition
    reduced_cell_formula = vasprun_dict['reduced_cell_formula']
    # n° atoms per formula unit
    nsites_pfu = 0
    for el in reduced_cell_formula:
        nsites_pfu += reduced_cell_formula[el]
    # n° of formula units in calculation
    n_formula_units = nsites/nsites_pfu
    total_energy_pfu = total_energy/ n_formula_units
    
    # writing computed phases dictionary
    computed_phases.update({phase:total_energy_pfu})
    
    file_table.write('%s  %f \n' %(phase  , total_energy_pfu))
    
    
    print(phase,total_energy_pfu)
    
    
file_table.close()    
    
###############################################################################    
print('')
print('Calculating Phase Diagram...\n')


for phase in computed_phases:
    
    # getting Composition Object
    comp = Composition(phase)
    # getting entry for PD Object
    entry = PDEntry(comp,computed_phases[phase])
    # building list of entries
    entries.append(entry)

# getting PD from list of entries
pd = PhaseDiagram(entries)

# get distance from convex hull for cubic phase
comp = Composition('NaNbO3')
energy = -38.26346361
entry = PDEntry(comp,energy)
cubic_instability = pd.get_e_above_hull(entry)

pd_dict = pd.as_dict()

# Getting Plot
plt = PDPlotter(pd, show_unstable=False)  # you can also try show_unstable=True

#plt_data = plt.pd_plot_data
# getting plot for chem potential - variables 'fontsize' for labels size and 'plotsize' for fig size have been added (not present in original pymatgen) to get_chempot_range_map_plot function
chem_pot_plot = plt.get_chempot_range_map_plot([Element("Na"), Element("Nb")], 
                                                fontsize = 14,plotsize=1.5)
#plt.write_image("chem_pot_{}.png".format('-'.join(system)), "png")
chem_pot_plot.savefig(f'chem_pot_{system_name}.png')  # save figure
# getting plot for PD - variables 'fontsize' for labels size and plotsize for fig size have been added (not present in original pymatgen) to get_plot function
pd_plot = plt.get_plot(label_stable = True,fontsize = 24,plotsize = 3)

pd_plot.savefig(f'PD_{system_name}.png')
