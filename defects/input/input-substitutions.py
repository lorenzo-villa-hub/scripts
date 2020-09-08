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
from pymatgen import PeriodicSite
from pymatgen.analysis.defects.core import Substitution
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
charge_states_dict = {'Sr':[0,1,2],
                      'Ca':[0,1,2]}

potcar_symbols = ['Na','Nb_pv','O']
###########################################################################

structure = Poscar.from_file('POSCAR_unit').structure
default_inputs = DefaultInputs(structure)
incar_settings = default_inputs.get_incar_default(xc='PBE')
incar_settings['LVTOT'] = '.TRUE.'
kpoints = Kpoints.gamma_automatic(kpts=(2,2,2))



supercell_size = 3
structure.make_supercell(supercell_size)

structure_pure = structure.copy() 

for el in charge_states_dict:
    structure = structure_pure.copy()
    for s in structure.sites:
        if s.species_string == 'Na':
            defect_site = PeriodicSite(el,s.frac_coords, s.lattice)
            break
        
    substitution = Substitution(structure, defect_site)
    defect_site_coords = defect_site.frac_coords    
    
    structure = substitution.generate_defect_structure()
    structure.get_sorted_structure()
    
    for charge in charge_states_dict[el]:
        path = os.path.join(os.getcwd(),f'{el}-substitution',f'Charged{charge}')
        
        default_inputs = DefaultInputs(structure)
        potcar_symbols = []
        for s in default_inputs.potcar_symbols:
            if s != 'Na_pv':
                potcar_symbols.append(s)
            else:
                potcar_symbols.append('Na')
        potcar = default_inputs.get_potcar(potcar_symbols=potcar_symbols,potcar_functional='PBE')
        val = {}
        for p in potcar:
            val[p.element] = p.nelectrons
        
        nelect = -1*charge + sum([ val[el]*structure.composition.as_dict()[el] for el in structure.composition.as_dict()])       
        #update NELECT
        incar_settings['NELECT'] = nelect
        
        # get and write scheme
        c = CalculationSchemes(structure,incar_settings=incar_settings,kpoints=kpoints,potcar=potcar,name=system_name+f'_{el}{charge}')
        scheme = c.pbe_rel(scheme_name='PBE')
        scheme.write_scheme(path=path, make_dir_if_not_present=True)
        
        # write info file
        with open(os.path.join(os.getcwd(),f'{el}-substitution','info'),'w') as info_file:
            info_file.write(f'#Coordinates of substituted atom for {el} substitution : \n')
            info_file.write('%f,%f,%f' %(defect_site_coords[0], defect_site_coords[1], defect_site_coords[2]))
                 
                 
                 

