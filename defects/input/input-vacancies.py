#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 25 16:49:50 2019

@author: villa
"""

# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 14:38:17 2019

@author: Lorenzo
"""


import os
from shutil import copyfile
import re

from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints, Potcar, VaspInput
from pymatgen.core.structure import Structure, SiteCollection, Lattice
from pymatgen.core.sites import Site
from my_functions.vasp.default_inputs import DefaultInputs
from my_functions.vasp.calculation_schemes import CalculationSchemes


##############################################################################
print('')
print('''Script that sets up files for vacancy calculations''')
print('''POSCAR file named "POSCAR_unit" must be present in work dir''')
print('''You must set up the desired CHARGE STATES for each vacancy type in the dictionary at the beginning of the script''')    
print('')

# system name to be inserted in job_vasp.sh
system_name = 'N'
# SETTING CHARGE STATE DICTIONARY
charge_states_dict = {'Na':[-1,0,1],
                      'Nb':[-5,-4,-3,-2,-1,0],
                      'O':[-2,-1,-0,1,2]}

potcar_symbols = ['Na','Nb_pv','O']
###########################################################################

structure = Poscar.from_file('POSCAR_unit').structure
default_inputs = DefaultInputs(structure)
incar_settings = default_inputs.get_incar_default(xc='HSE06',aexx=0.25)
incar_settings['LVTOT'] = '.TRUE.'
kpoints = Kpoints.gamma_automatic(kpts=(2,2,2))

if potcar_symbols:
    potcar= Potcar(potcar_symbols,functional='PBE')
else:
    potcar=default_inputs.get_potcar(potcar_symbols=None,potcar_functional='PBE')
val = {}
for p in potcar:
    val[p.element] = p.nelectrons

supercell_size = 3
structure.make_supercell(supercell_size)

structure_pure = structure.copy() 

for el in charge_states_dict:
    structure = structure_pure.copy()
    # choose vacancy site
    for site in structure.sites:
        if site.specie.name == el:
            removed_site = site
            atom_index = structure.sites.index(site)
            break
    #remove atom
    rem_atom_coord = removed_site.frac_coords
    structure.remove_sites([atom_index]) 
    
    for charge in charge_states_dict[el]:
        path = os.path.join(os.getcwd(),f'{el}-vacancy',f'Charged{charge}')
        nelect = -1*charge + sum([ val[el]*structure.composition.as_dict()[el] for el in structure.composition.as_dict()])       
        #update NELECT
        incar_settings['NELECT'] = nelect
        
        # get and write scheme
        c = CalculationSchemes(structure,incar_settings=incar_settings,kpoints=kpoints,potcar=potcar,name=system_name+f'_{el}{charge}')
        scheme = c.hse_rel_gamma(scheme_name='HSE')
        scheme.write_scheme(path=path, make_dir_if_not_present=True)
        
        # write info file
        with open(os.path.join(os.getcwd(),f'{el}-vacancy','info'),'w') as info_file:
            info_file.write(f'#Coordinates of removed atom for {el} vacancy : \n')
            info_file.write('%f,%f,%f' %(rem_atom_coord[0], rem_atom_coord[1], rem_atom_coord[2]))
                 
                 
                 

