#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Sep 23 11:03:36 2019

@author: villa
"""


import json

from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from my_functions.phase_diagram.analysis import Chempots, ChempotAnalysis, PDHandler
from my_functions.phase_diagram.experimental import ChempotExperimental
       
print('\nReading phase diagram entries from file...\n')

with open('PD_Na-Nb-O_NN_cubic.json') as json_file:
	computed_phases = json.load(json_file)

pd = PDHandler(computed_phases).phase_diagram()
chempot_analisys = ChempotAnalysis(computed_phases)

comp = Composition('NaNbO3')
chempots = pd.get_all_chempots(comp)
   
for p in chempots:
    #order by element
    chempots[p] = {k: v for k, v in sorted(chempots[p].items(), key=lambda item: item[0])}

A = 'NaNbO3-Na8Nb5O14-Na7NbO6'
B = 'NaNbO3-NbO2-Nb2O5'
C = 'NaNbO3-Nb2O5-O2'
D = 'NaNbO3-Na7NbO6-Na2O2'

chempots_boundary = {'A':chempots[A],'B':chempots[B],'C':chempots[C],'D':chempots[D]}
chempots_boundary['E'] = chempot_analisys.get_chempots_abs({Element('Na'):-1.8, Element('Nb'):-6, Element('O'):-1.95})

chempots_boundary_dict = {}
for res in chempots_boundary:
    chempots_boundary_dict[res] = Chempots(chempots_boundary[res]).as_dict()
   

with open('chempots_boundary.json','w') as file:
    json.dump(chempots_boundary_dict,file)


exp = ChempotExperimental(temperature=950,partial_pressure=0.2)
mu_ox = exp.chempot_ideal_gas(exp.oxygen_standard_chempot())    
chempots = chempot_analisys.boundary_analysis(comp, fixed_chempot_delta={Element('O'):mu_ox})

X1 = 'Nb2O5-NaNbO3'
X3 = 'NaNbO3-Na7NbO6'
chempots_exp = {'X1':chempots[X1],'X3':chempots[X3]}
chempots_exp['X2'] = {}
for el in chempots_exp['X1']:
    chempots_exp['X2'][el] = (chempots_exp['X1'][el] + chempots_exp['X3'][el])/2
    
chempots_exp = {res : chempot_analisys.get_chempots_abs(chempots_exp[res]) for res in chempots_exp}

chempots_exp_dict = {}
for res in chempots_exp:
    chempots_exp_dict[res] = Chempots(chempots_exp[res]).as_dict()


with open('chempots_experimental.json','w') as file:
    json.dump(chempots_exp_dict,file)

  
    
