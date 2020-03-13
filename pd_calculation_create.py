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

###############################################################################
# function to write job_vasp with customized job_name 

def write_job_vasp (input_dir = './', output_dir='./', job_name = ''):
     # setting up job_vasp.sh script
     new_job_vasp_file = open(output_dir + 'job_vasp.sh','w')
     
     import re
     with open(input_dir + 'job_vasp.sh') as origin_file:
           # searching lines in input file   
           for line in origin_file:
              # emulating 'grep' command
              job_line = re.findall(r'#SBATCH --job-name=', line)
              # if line is the job line print customized job name                      
              if job_line:
                  # writing line with new job name
                  new_job_vasp_file.write(f'#SBATCH --job-name={job_name} \n')
              else:
                  # if line doesn't contain job-name write same line from reference job_vasp.sh
                  new_job_vasp_file.write(line)                             
           new_job_vasp_file.close() 

###############################################################################

MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

# Create phase diagram!
unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections
pd = PhaseDiagram(processed_entries)
pd_dict = pd.as_dict()
# Plot!
plt = PDPlotter(pd, show_unstable=False)  
# you can also try show_unstable=True
#plt.get_chempot_range_map_plot([Element("Na"), Element("Nb")])
#plt.show()

# create dictionary for stable phases and material ID
stable_phases = {}
stable_entries = pd.stable_entries

print ('Stable Entries (formula, materials_id)\n--------')
for e in pd.stable_entries:
    print (e.composition.reduced_formula, e.entry_id)
    stable_phases.update({e.composition.reduced_formula : e.entry_id})
###############################################################################
    
print('')
print('Getting structures from MP...')
print('')

# INCAR
incar_dict = {
#"KPAR": 4,
#"NPAR": 24,
# "NBANDS": 96,
"ISTART": 0,
"ICHARG": 2,
"IBRION": 2,
"NSW": 100,
"ISIF": 3,
"EDIFFG": -0.05,
"ISPIN": 1,
"LWAVE": ".TRUE.",
"LCHARG": ".TRUE.",
"LORBIT":10,
"ENCUT": 500,
"EDIFF": 1e-06,
"ISMEAR": 0,
"SIGMA": 0.05,
"ALGO": "All",
"ISYM": 2,
"AMIX": 0.2,
"LREAL": ".FALSE.",
# "LVTOT": ".TRUE."        
# "NEDOS": 2000,
#    "LHFCALC" : ".TRUE.",
#    "HFSCREEN": 0.2,
#    "NKRED": 2,
#    "PRECFOCK": "Fast",
#    "AEXX": 0.24,
}  

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
#data = mpr.get_data('NaNbO3')

list_file = open('phases_list','w')
list_file.write('Phase    Space Group   Materials ID\n')

for phase in stable_phases:
    
    
    
    comp = Composition(phase).as_dict()
    
    file_dir = './' + phase + '/'
    
    # checking if path already exists - if not create it
    if not os.path.exists(file_dir):
            os.makedirs(file_dir)
    else:
         print(f'{file_dir} already exists\n-----')
    
    # writing materials ID in file
    mid_file = open(file_dir + 'material-ID','w')
    
    struct = mpr.get_structure_by_material_id(stable_phases[phase], 
                                              final = False,
                                              conventional_unit_cell = True)
    # getting space group of structure
    spacegroup = struct.get_space_group_info()
 #   prova = mpr.get_entry_by_material_id(stable_phases[phase] , inc_structure = 'final').as_dict()
    
    print(f'Structure acquired :\n - Reduced formula = {phase} , Space group = {spacegroup},  Materials ID = {stable_phases[phase]}  \n --------')   
    #print(struct)
    print('')
    list_file.write(f'{phase} : {spacegroup}   {stable_phases[phase]}\n')
    mid_file.write(f'{stable_phases[phase]}')
    mid_file.close()
    
    # POSCAR
    poscar = Poscar(struct)
    poscar_dict = poscar.as_dict()
    
    # KPOINTS    
    k = 7
    kpoints = Kpoints.gamma_automatic(
    kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
     )
        
    # creating Incar object from 'incar_dict'
    incar = Incar(incar_dict)
        
    # get POTCAR with right order from POSCAR
    # check for prevoius element - if it's the same don't write it twice
    prevoius_el = None
    # initializing potcar_symbols
    potcar_symbols = []
    #getting sites list
    sites = poscar_dict['structure']['sites']
    # getting label for element on site
    for site_index in range(0,len(sites)) :
        el = sites[site_index]['label']
        # write only if it is different from the prevoious one
        if prevoius_el != el:                    
            potcar_symbols.append(potcar_choices[el])
            prevoius_el = el
    # set Potcar object        
    potcar = Potcar(symbols=potcar_symbols, functional='PBE', sym_potcar_map=None)

    VASP_input = VaspInput(incar,kpoints,poscar,potcar)
    VASP_input.write_input(file_dir, make_dir_if_not_present=False)
    
    # job_vasp.sh
    write_job_vasp(output_dir = file_dir, job_name = phase)
    
    
    
    
list_file.close()    
