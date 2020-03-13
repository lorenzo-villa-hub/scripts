#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 18:23:10 2019

@author: villa
"""

import os

from pymatgen.io.vasp.inputs import Incar, Poscar, Potcar, Kpoints, VaspInput
from pymatgen.analysis.defects.core import Vacancy
from pymatgen.core.structure import Structure
from my_functions.write_job_vasp import write_job_vasp

###############################################################################
# INPUT
###############################################################################

# system name
system_name = 'N'
# name of XC functional to use
xc_func = 'HSE'

# POTCAR names
potcar_choices = {'Na':'Na',
                  'Nb':'Nb_pv',
                  'O':'O'}

# types of defects to generate
defect_types = ['Na','Nb','O']

# SETTING CHARGE STATE DICTIONARY
charge_states_dict = {'Na':[-1,0,1],
                      'Nb':[-5,-4,-3,-2,-1,0,1],
                      'O':[-2,-1,-0,1,2]}

# names for folders of pure structure
step_pure_names = {'4-PBE-DC/':['electronic/','ionic/']}
# names for folders of defects structure
step_def_names = ['1-PBE-SCF/','2-PBE-OPT/']

# variable for creation of file for Pure structure
create_pure = False
# variable for creation of files for defects
create_defects = True

###############################################################################
###############################################################################

# set up common INCAR - dictionary
incar_dict = {
        
    "# KPAR": 4,
    "# NPAR": 24,
    "NBANDS": 480,
    "## ISTART": 0,
    "## ICHARG": 2,
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
    "ISYM": 3,
    "AMIX": 0.2,
    "LREAL": ".FALSE.",
    "LVTOT": ".TRUE.",
    "NELM": 500,

  # PBE+U
     
#    "NEDOS": 2000,
#    "LDAU" : ".TRUE.",
#    "LDAUU": "0 11 0",
#    "LDAUTYPE": 2,
#    "LDAUPRINT": 2,
     
  # HSE
  
    "NEDOS": 2000,
    "LHFCALC" : ".TRUE.",
    "HFSCREEN": 0.2,
    "NKRED": 2,
    "PRECFOCK": "Fast",
    "AEXX": 0.24,
    "SYMPREC": 1e-04
 }     
#############################################################################

# supercell size
s = 3

unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure
struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
Poscar(struct).write_file("POSCAR_supercell")

# set up POTCAR
potcar_symbols = []
type_elements = struct.symbol_set
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
    

if create_defects == True:

    for el in defect_types:
        
         # setting path
         element_path =  (f'./{el}-vacancy/')
        # checking if path already exists - if not create it
         if not os.path.exists(element_path):
                os.makedirs(element_path)          
         #opening info file
         info_written = False
         info_file = open(element_path + 'info', 'w')   
         
         # iteration over charge states
         for q in charge_states_dict[el]:
            charge_path = element_path + f'Charged{q}/'         
            if not os.path.exists(charge_path):
                os.makedirs(charge_path)
            else:
                print(f'Directory {charge_path} already exists')
             
            for step in step_def_names:   
                if not os.path.exists(charge_path + step):   
                    os.makedirs(charge_path + step)
            
            # creating vacancy object
            # first chosing site for vacancy
            for site in struct.sites:
                if site.specie.symbol == el:
                    vacancy_site = site
                    break
                
            vacancy_object = Vacancy(struct, vacancy_site, charge = q)
            rem_atom_coord = vacancy_site.frac_coords
            
            if info_written == False:
                # printing coordinates of removed atom
                info_file.write(f'#Coordinates of removed atom for {el} vacancy : \n')
                info_file.write('%f,%f,%f' %(rem_atom_coord[0], rem_atom_coord[1], rem_atom_coord[2]))
                info_file.close()
                info_written = True
                
            # creating defect structure
            struct_vac = vacancy_object.generate_defect_structure()        
            # set up POSCAR 
            poscar = Poscar(struct_vac)        
            # set up KPOINTS
            k = 2
            kpoints = Kpoints.gamma_automatic(
            kpts=(k, k, k), shift=(0.0, 0.0, 0.0))        
            # set up INCAR
            # calculating NELECT 
            nelect = -1*q
            for site in struct_vac.sites:
                nelect += val_el_dict[site.specie.symbol]
    
            incar_dict.update({'NELECT':nelect})
            incar = Incar(incar_dict)        
            # write VASP input
            vasp_input = VaspInput(incar,kpoints,poscar,potcar)
            vasp_input.write_input(charge_path + step_def_names[0])
            # write job_vasp.sh    
            write_job_vasp(output_file = charge_path+step_def_names[0]+'job_vasp.sh', job_name = f'{system_name}_{xc_func}_{el}{q}_1')
    
            if len(step_def_names) > 1:
                 # changing INCAR for step 2
                 incar_dict['ISTART'] = 1
                 incar_dict['ICHARG'] = 1
                 incar_dict['EDIFF'] = 1e-05
                 incar_dict['NSW'] = 100
                 #write INCAR for step 2
                 incar = Incar(incar_dict)
                 # write VASP input
                 vasp_input = VaspInput(incar,kpoints,poscar,potcar)
                 vasp_input.write_input(charge_path + step_def_names[1])
                 # write job_vasp.sh
                 write_job_vasp(output_file = charge_path+step_def_names[1]+'job_vasp.sh', job_name = f'{system_name}_{xc_func}_{el}{q}_2')
    





