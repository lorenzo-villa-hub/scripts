#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 23 11:03:36 2019

@author: villa
"""

import copy
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import json

from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter , PDEntry, GrandPotentialPhaseDiagram, GrandPotPDEntry
from pymatgen.electronic_structure.dos import CompleteDos

from my_functions.phase_diagram.analysis import ChempotAnalysis, PDHandler
from my_functions.phase_diagram.experimental import ChempotExperimental
from my_functions.defects.analysis import DefectsAnalysis
       
print('\nReading phase diagram entries from file...\n')

with open('PD_Na-Nb-O_NN_cubic.json') as json_file:
	computed_phases = json.load(json_file)

ph = PDHandler(computed_phases)    
c = ChempotAnalysis(computed_phases)
mu_ref_ox = c.chempots_reference[Element('O')]


entries = ph.pd_entries()
temperature = 950
#partial_pressure = 0.2

pressures = [] # list of partial pressures
reservoirs = [] # list of reservoirs, indexes are coherent with relative partial pressures
for p in np.arange(1,10.1,0.1):

    partial_pressure = p/10
    exp = ChempotExperimental(temperature,partial_pressure)
    mu0_ox = exp.oxygen_standard_chempot()
    mu_delta_ox = exp.chempot_ideal_gas(mu0_ox)        
    mu_abs_ox = mu_ref_ox + mu_delta_ox 
    
    comp1 , comp2 = c.get_composition_boundaries(Composition('NaNbO3'),{Element('O'):mu_abs_ox})
    chempots_delta = c.get_chempots_boundary(comp1,comp2,{Element('O'):mu_delta_ox})
    chempots_abs = {el:chempots_delta[el] + c.chempots_reference[el] for el in chempots_delta}
    
    pressures.append(np.around(partial_pressure,decimals=4))
    reservoirs.append(chempots_abs)
   
print('Reading defect calculation data...\n')    
with open('vacancies_NN_HSE.json') as json_file:
	data = json.load(json_file)    
defects_analysis = DefectsAnalysis.from_dict(data)
    
print('Reading DOS...\n')
with open('dos_NN_HSE.json') as json_file:
	data = json.load(json_file)
bulk_dos = CompleteDos.from_dict(data)


concentrations = []
for r in reservoirs:
    
    fermi_energy = defects_analysis.equilibrium_fermi_level(r,bulk_dos)
    cn = defects_analysis.defect_concentrations_stable_charges(r,temperature=temperature,fermi_level=fermi_energy)
    concentrations.append(cn)
    
data = {'partial_pressures':pressures,'defect_concentrations':concentrations}
  
with open('defect_concentrations_NN_HSE.json','w') as json_file:
	json.dump(data,json_file)   
    
    
