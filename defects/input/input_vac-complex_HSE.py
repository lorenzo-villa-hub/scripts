#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 28 14:11:40 2019

@author: villa
"""
# script to create input for complex of 1 vacancy of oxygen and 2 vacancies of Na in NaNbO3


###############################################################################
# INPUT
###############################################################################

# system name
system_name = 'N'
# name of XC functional to use
xc_func = 'HSE'

charge_states = [-1,0,1]

# POTCAR names
potcar_choices = {'Na':'Na',
                  'Nb':'Nb_pv',
                  'O':'O'}

# names for folders of defects structure
step_def_names = ['1-PBE-SCF-Gamma/','2-PBE-OPT-Gamma/','3-HSE-SCF-Gamma/',
                  '4-HSE-OPT-Gamma/','5-PBE-SCF/','6-HSE-SCF/','7-HSE-OPT/']

###############################################################################
###############################################################################

# set up common INCAR - dictionary
incar_dict = {    
  #  "NPAR": 24,
    "NBANDS": 500,
    "###  ISTART": 0,
    "###  ICHARG": 2,
    "IBRION": 2,
    "NSW": 0,
    "ISIF": 2,
    "EDIFFG": -0.05,
    "ISPIN": 1,
    "LWAVE": ".TRUE.",
    "LCHARG": ".TRUE.",
    "LORBIT":11,
    "ENCUT": 500,
    "EDIFF": 1e-06,
    "ISMEAR": 0,
    "SIGMA": 0.05,
    "ALGO": "All",
    "ISYM": 2,
    "AMIX": 0.2,
    "LREAL": ".FALSE.",
    "LVTOT": ".TRUE.",
    
#    "NEDOS": 2000,
#    "LDAU" : ".TRUE.",
#    "LDAUU": "0 11 0",
#    "LDAUTYPE": 2,
#    "LDAUPRINT": 2,
     
#    "NEDOS": 2000,
#    "LHFCALC" : ".TRUE.",
#    "HFSCREEN": 0.2,
#    "NKRED": 2,
#    "PRECFOCK": "Fast",
#    "AEXX": 0.24
 }     
#############################################################################


import os
import numpy as np
from pymatgen.core.structure import Structure, IStructure
from pymatgen.io.vasp.inputs import Poscar,Potcar,Kpoints,Incar,VaspInput

from my_functions.write_job_vasp import write_job_vasp

path = '.' + '/'

incar_dict_init = incar_dict.copy()
# get supercell structure
structure = Poscar.from_file('POSCAR_supercell').structure

# set up POTCAR
potcar_symbols = []
type_elements = structure.symbol_set
val_el_dict = {}
for el in type_elements:
    potcar_symbols.append(potcar_choices[el])   

# creating POTCAR object    
potcar = Potcar(symbols=potcar_symbols, functional='PBE', sym_potcar_map=None)

# dictionary of valence electrons    
for el in type_elements:
    for potcar_single in potcar:
        if potcar_single.element == el:
            val_el_dict[el] = potcar_single.nelectrons
            break
    

# define first vacancy site
first_vacancy_site = structure.sites[118]
# find neighbors of this site
neighbors = structure.get_neighbors(first_vacancy_site,2.9)
# create list of tuples as: (Site object,site_index in Structure object)
vacancy_sites = [(first_vacancy_site,118)]
# define counter for number of other vacancies
count = 1
total_number_of_vacancies = 3
for n in neighbors:
    site = n[0]
    if site.specie.symbol == 'Na':
        for s in structure.sites:
            if np.array_equiv(s.frac_coords,site.frac_coords):
                vacancy_sites.append((site,structure.sites.index(s)))
        count += 1
        if count == total_number_of_vacancies:
            break

print('')        
print('Vacancy sites: \n')
for v in vacancy_sites:
    print(v[0].frac_coords,v[0].specie.symbol)
    structure.remove_sites([v[1]])

# final k-mesh size
k=2
###############################################################################
# set up files

for q in charge_states:
    
 # reset initial INCAR
     incar_dict = incar_dict_init.copy()
              
     charge_path = path + f'Charged{q}/'         
     if not os.path.exists(charge_path):
        os.makedirs(charge_path)
     else:
        print(f'Directory {charge_path} already exists')
     
     for step in step_def_names:   
         if not os.path.exists(charge_path + step):   
            os.makedirs(charge_path + step)
        
     #####################################################################  
     # set up POSCAR
     poscar = Poscar(structure)
     #####################################################################
     # set up KPOINTS
     # KPOINTS
     kpoints = Kpoints.gamma_automatic(
     kpts=(1, 1, 1), shift=(0.0, 0.0, 0.0)
     )         
     
     #####################################################################        
     # set up INCAR for step 1 and 2
     # calculate total n° of electrons accounting for charge state
     # calculating NELECT 
     nelect = -1*q
     for site in structure.sites:
        nelect += val_el_dict[site.specie.symbol]
     #update NELECT in INCAR 
     incar_dict.update({'NELECT': nelect})
     # write INCAR for step 1
     incar = Incar(incar_dict)             
     vasp_input = VaspInput(incar,kpoints,poscar,potcar)
     vasp_input.write_input(output_dir = charge_path+step_def_names[0], make_dir_if_not_present = False)
     
     #step 2 - PBE-OPT-G
     if len(step_def_names) > 1:
         # changing INCAR for step 2
         incar_dict['EDIFF'] = 1e-05
         incar_dict['NSW'] = 100
         #write INCAR for step 2
         incar = Incar(incar_dict)
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[1], make_dir_if_not_present = False)
     # step 3 - HSE-SCF-G 
     if len(step_def_names)> 2:
         incar_dict['EDIFF'] = 1e-06
         incar_dict['NSW'] = 0
         incar_dict['ISYM'] = 3
         incar_dict.update({
                    "LHFCALC" : ".TRUE.",
                    "HFSCREEN": 0.2,
                    "PRECFOCK": "Fast",
                    "AEXX": 0.24
                     })
         incar = Incar(incar_dict)
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[2], make_dir_if_not_present = False)
         
     # step 4 - HSE-OPT-G    
     if len(step_def_names)> 3:
         incar_dict['EDIFF'] = 1e-05
         incar_dict['NSW'] = 100
         incar = Incar(incar_dict)
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[3], make_dir_if_not_present = False)
         
     # step 5 - PBE-SCF    
     if len(step_def_names)> 4:
         # resetting original INCAR (removing HSE parts)
         incar_dict = incar_dict_init.copy()
         incar_dict.update({'NELECT': nelect})
         incar_dict["KPAR"] = 4
         incar = Incar(incar_dict)
         # changing k points mesh
         kpoints = Kpoints.gamma_automatic(
         kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
         )
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[4], make_dir_if_not_present = False)
     
     # step 6 - HSE-SCF   
     if len(step_def_names)> 5:
         incar_dict['ISYM'] = 3
         incar_dict.update({
                    "LHFCALC" : ".TRUE.",
                    "HFSCREEN": 0.2,
                    "NKRED": 2,
                    "PRECFOCK": "Fast",
                    "AEXX": 0.24
                    })
         incar = Incar(incar_dict)
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[5], make_dir_if_not_present = False)             
     
        # step 7 - HSE-OPT   
     if len(step_def_names)> 6:
         incar_dict['EDIFF'] = 1e-05
         incar_dict['NSW'] = 100
         incar = Incar(incar_dict)
         vasp_input = VaspInput(incar,kpoints,poscar,potcar)
         vasp_input.write_input(output_dir = charge_path+step_def_names[6], make_dir_if_not_present = False) 
         
     # writing job_vasp.sh for every step 
     for step in step_def_names:
         step_number = step_def_names.index(step) + 1
         if 'HSE-OPT' in step:
             write_job_vasp(input_file='job_vasp_array.sh',
                            output_file=charge_path+step+'job_vasp_array.sh',
                            job_name = f'{system_name}_VNa2O{q}_{step_number}')
         else:
             write_job_vasp(input_file='job_vasp.sh',
                            output_file=charge_path+step+'job_vasp.sh',
                            job_name = f'{system_name}_VNa2O{q}_{step_number}')             


















