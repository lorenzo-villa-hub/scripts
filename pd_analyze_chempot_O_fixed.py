#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 23 11:03:36 2019

@author: villa
"""

import os
import sys
import copy
import numpy as np
from glob import glob
import matplotlib.pyplot as plt
import matplotlib

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.core.composition import Composition
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter , PDEntry, GrandPotentialPhaseDiagram
from pymatgen.core.periodic_table import Element

system = ['Na','Nb','O']

system_name = '-'.join(system)


    
###############################################################################    
print('')
print('Calculating Phase Diagram...\n')

computed_phases = {}

''' Read phase diagram Entries from file'''

entries_file = open('PD_Na-Nb-O.dat','r')

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
    # using data for cubic phase (from B-M data)
    if phase == 'NaNbO3':
        computed_phases[phase] = -46.50829780
    # getting entry for PD Object
    entry = PDEntry(comp,computed_phases[phase])
    # building list of entries
    entries.append(entry)

# getting PD from list of entries
pd = PhaseDiagram(entries)

phase = 'NaNbO3'
comp = Composition(phase)
chem_potentials = pd.get_all_chempots(comp)

# copy dictionary to save absolute chemical potentials
chem_potentials_abs = copy.deepcopy(chem_potentials)
#
for corner in chem_potentials:
    for el in chem_potentials[corner]:
        el_dict = el.as_dict()
        el_name = el_dict['element']
        
        if el_name != 'O':
            chem_potentials[corner][el] += (-1)*(computed_phases[el_name])
        else:
            chem_potentials[corner][el] += (-1)*(computed_phases['O2']/2)
#    
 
#print('NN_HSE_aexx25')           
#for zone in chem_potentials:
#    
#    print('')
#    print(zone)
#    print(chem_potentials[zone])    


NN_entry = entries[0]
NN_form_energy = pd.get_form_energy(NN_entry)

def chem_pot_O_fixed(mu_el, NN_form_energy, mu_O=-1.09):
    mu_variable = NN_form_energy - mu_el - mu_O

    return mu_variable

mu_Na_min,mu_Na_max = pd.get_chempot_range_stability_phase(comp,Element('Na'))[Element('Na')]
mu_Nb_min,mu_Nb_max = pd.get_chempot_range_stability_phase(comp,Element('Na'))[Element('Nb')]

mu_Nb_min += -1* computed_phases['Nb']
mu_Nb_max += -1* computed_phases['Nb']

mu_Na_min += -1* computed_phases['Na']
mu_Na_max += -1* computed_phases['Na']

mu_Na = np.arange(mu_Na_min-5,mu_Na_max,(mu_Na_max-mu_Na_min)/50)


mu_O = -1.09

mu_Na_max_O_fixed = chem_pot_O_fixed(mu_Nb_min,NN_form_energy)
mu_Na_min_O_fixed = mu_Na_min

mu_Nb_min_O_fixed = mu_Nb_min
mu_Nb_max_O_fixed = chem_pot_O_fixed(mu_Na_min,NN_form_energy)

size=1
plt.figure(figsize=(10*size,6*size))
matplotlib.rcParams.update({'font.size': 15*size})

plt.plot(mu_Na,chem_pot_O_fixed(mu_Na,NN_form_energy,mu_O=mu_O), linewidth= 3*size, label='$\Delta \mu_{O}$ = '+ f'{mu_O} eV')


plt.xlim(mu_Na_min-0.5,mu_Na_max)
plt.axhspan(mu_Nb_min,mu_Nb_max_O_fixed,color='k',alpha=0.2,label='stability of NaNbO$_{3}$')

plt.ylim(chem_pot_O_fixed(mu_Na_max,NN_form_energy,mu_O=mu_O), chem_pot_O_fixed(mu_Na_min,NN_form_energy,mu_O=mu_O) +0.5)

plt.xlabel('$\Delta \mu_{Na}$ (eV)',fontsize=25)
plt.ylabel('$\Delta \mu_{Nb}$ (eV)',fontsize=25)
plt.legend()

points = {'X1':(mu_Na_min_O_fixed,mu_Nb_max_O_fixed),
          'X2':(mu_Na_min_O_fixed + (mu_Na_max_O_fixed-mu_Na_min_O_fixed)/2 ,
                  chem_pot_O_fixed(mu_Na_min_O_fixed + (mu_Na_max_O_fixed-mu_Na_min_O_fixed)/2,NN_form_energy)) , 
          'X3':(mu_Na_max_O_fixed,mu_Nb_min_O_fixed) 
        }

for point in points:
    plt.scatter(points[point][0],points[point][1], color='', edgecolor='k', linewidths=3, s=500)
    plt.text(points[point][0]+0.2,points[point][1],point)


plt.savefig('mu_etls_O_fixed_exp.pdf')
#def chem_pot_O_fixed(entry,mu_O_rel):
#    chem_pot_O_fixed
    


##############################################################################
## PLOTTING
#
## get distance from convex hull for cubic phase
##comp = Composition('NaNbO3')
##energy = -38.26346361
##entry = PDEntry(comp,energy)
##cubic_instability = pd.get_e_above_hull(entry)
#
##pd_dict = pd.as_dict()
#
## Getting Plot
#plt = PDPlotter(pd, show_unstable=False)  # you can also try show_unstable=True
#
##plt_data = plt.pd_plot_data
## getting plot for chem potential - variables 'fontsize' for labels size and 'plotsize' for fig size have been added (not present in original pymatgen) to get_chempot_range_map_plot function
#chem_pot_plot = plt.get_chempot_range_map_plot([Element("Na"), Element("Nb")], 
#                                                fontsize = 14,plotsize=1.8)
#
#chem_pot_plot.savefig(f'chem_pot_{system_name}_NN_R3-_aexx25.pdf')  # save figure
## getting plot for PD - variables 'fontsize' for labels size and plotsize for fig size have been added (not present in original pymatgen) to get_plot function
##pd_plot = plt.get_plot(label_stable = True,fontsize = 24,plotsize = 3)
#
##pd_plot.savefig(f'PD_{system_name}.png')
#
################################################################################