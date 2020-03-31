#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Sep 19 16:41:13 2019

@author: villa
"""
import os
from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints , Potcar, VaspInput
from pymatgen.io.vasp.sets import VaspInputSet , DictSet
from pymatgen.core.composition import Composition
from pymatgen import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter
from pymatgen.core.periodic_table import Element
from my_functions.materials_project import MPDatabase
from my_functions.vasp.default_inputs import DefaultInputs
from my_functions.vasp.calculation_schemes import CalculationSchemes

print('-----')
print(' Script to generate input files with pymatgen for VASP calculation of phase diagrams\n------')
print('Please enter system for which you want to calculate the PD and \n the choices for the POTCAR files at the beginning of the script\n------')
print('This script retrieves data for PD from Materials Project to get\n the stable phases for a given system')
print('The structure of the detected stable phases are retrieved from \n the materials project DB and used to generate input files\n------')
print('The parameters for INCAR and KPOINTS are set in the script')

##############  PLEASE EDIT HERE ##############################################
system = ["Na", "Nb" , "O"]  # system we want to get PD for

potcar_choices = {'Na':'Na',
                  'Nb':'Nb_pv',
                  'O':'O'}
###############################################################################

mp_database = MPDatabase()
MAPI_KEY = mp_database.api_key  

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

# Create phase diagram!
unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections
pd = PhaseDiagram(processed_entries)

# create dictionary for stable phases and material ID
material_ids = {}
stable_entries = pd.stable_entries

print ('Stable Entries (formula, materials_id)\n--------')
for e in pd.stable_entries:
    print (e.composition.reduced_formula, e.entry_id)
    material_ids.update({e.composition.reduced_formula : e.entry_id})
###############################################################################
    
print('')
print('Getting structures from MP...')
print('')


f_phases_list = open('phases_list','w')
f_phases_list.write('Phase    Space Group   Materials ID\n')

for phase in material_ids:
    
    path = os.path.join(os.getcwd(),'test-PD',phase)
    
    mp_database.mp_id = material_ids[phase]
    structure = mp_database.get_structure()
    # getting space group of structure
    spacegroup = structure.get_space_group_info()
    
    print('Structure acquired :\n - Reduced formula = %s, Space group = %s, Materials ID = %s \n --------' %(phase, spacegroup, material_ids[phase]))   
    print('')
    
    default_inputs = DefaultInputs(structure)
    potcar_symbols = [potcar_choices[el.symbol] for el in structure.composition.elements] # get potcar symbols from potcar choices dict and elements in structure
    potcar = default_inputs.get_potcar(potcar_symbols=potcar_symbols)
    incar_settings = default_inputs.get_incar_default(xc='PBE')
    
    calculation_schemes = CalculationSchemes(structure,incar_settings=incar_settings,potcar=potcar,name=phase)
    calculation_schemes.pbe_vol_rel().write_scheme(path=path,make_dir_if_not_present=True)
    
    f_phases_list.write('%s : %s %s\n' %(phase, spacegroup, material_ids[phase]))
    f_mid = open(os.path.join(path,'material-ID'),'w')
    f_mid.write('%s'%(material_ids[phase]))
    f_mid.close()
    
    
f_phases_list.close()    
