#!/nfshome/villa/anaconda3/bin/python
# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 14:34:20 2019

@author: villa
"""
import sys
import numpy as np
from glob import glob
import os
import matplotlib.pyplot as plt

from pymatgen.core.periodic_table import Element
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.thermodynamics import DefectPhaseDiagram
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.dos import CompleteDos

from my_functions.grep import grep
############################################################################
print('')
print('')
print('Usage : Ef_plot_defects_corr.py   path_to_def_calc_folde  [path_to_freysold_corr_folder]\n')
print('Please give delta chemical potentials in the different reservoirs in "chempot" dictionary\n')
print('Please give reference chemical potentials in the "chempot_ref" dictionary\n')
print('Please give path of pure calculation (unit cell or supercell both work) in the script\n')
print('Be careful that the naming of the folders is correct :')
print('     yourdata/defect_system/{element}-vacancy/Charged{charge}/2-PBE-OPT/vasp_data \n')
print('')  
print('')

#############################################################################
############ INPUT ##########################################################
#############################################################################

# delta chemical potentials in different reservoirs
chempot =   {'A':{'Na':-1.27,
                  'Nb':-1.36,
                  'O':-3.15},
             'B':{'Na':-1.60,
                  'Nb':-1.30,
                  'O':-3.06},
             'C':{'Na':-3.13,
                  'Nb':-8.94,
                  'O':  0},
             'D':{'Na':-2.84,
                  'Nb':-9.22,
                  'O':  0},
             'E':{'Na':-2.2,
                  'Nb':-5,
                  'O':-1.52}
              }
# reference chemical potentials
chempot_ref = {'Na':-1.295284,
               'Nb':-10.079199,
               'O': -9.879307/2}


# path of pure DOS calculation
pure_path = '/nfshome/villa/local-data/NN_cubic/U-tuning-new/U_tuning/U_11' + '/'
# getting and saving plots
get_plots = True 

###############################################################################
###############################################################################
print(f'Path of pure calculation: "{pure_path}"\n')

# folder with defects calculations
input_path = sys.argv[1] + '/'
system_name = os.path.basename(os.path.dirname(input_path))

# if freysold_corr_path was in input get path for freysold corrections folder
if len(sys.argv) > 2:
    input_frey = sys.argv[2]
    print(f'Path for Freysold corrections: "{input_frey}"\n')
else:
    input_frey = None
    print('No Freysold corrections included...\n')

# getting pure structure
structure_pure = Poscar.from_file(input_path+"POSCAR_supercell").structure
# n° of atoms in supercell structure
natoms_sup = len(structure_pure.sites)

# getting values of pure structure
vasprun_pure = Vasprun(pure_path + 'vasprun.xml')
# getting band_gap and vbm
(band_gap,cbm, vbm, is_band_gap_direct) = vasprun_pure.eigenvalue_band_properties
# n° atoms in pure structure calculation
natoms_pure = len(Poscar.from_file(pure_path+'POSCAR').structure.sites)
Epure = ((vasprun_pure.final_energy)/natoms_pure)*natoms_sup
# creating a list of directories contained in input directory
list_dir_vacancies = glob(input_path + '*/')

# creating defect entries
defect_entries = []  

for vacancy_dir in list_dir_vacancies:
    if '-vacancy' in vacancy_dir:
        vacancy_type = os.path.basename(os.path.dirname(vacancy_dir)).replace('-vacancy','')
        # reading info file
        with open(vacancy_dir+'info','r') as info_file:
            coord = info_file.readlines()[1].split(',')
            defect_site_coord = np.around(np.asarray(coord,dtype = np.float64), decimals = 4)
            
        # finding defect site
        for site in structure_pure.sites:
            if np.array_equiv(np.around(site.frac_coords,4),defect_site_coord):
                defect_site = site
                
        # dictionary of total energies        
        total_energies_dict = {}

        list_dir_charges = glob(vacancy_dir+'*/')

        # search in every directory of the list
        for charge_dir in list_dir_charges:
            # finding charge state from subfolder name
            q = int(os.path.basename(os.path.dirname(charge_dir)).replace('Charged',''))
            # reading vasprun.xml file
            vasprun = Vasprun(charge_dir + '2-PBE-OPT/vasprun.xml',ionic_step_skip=None, ionic_step_offset=0,
                              parse_dos=False, parse_eigen=False, parse_projected_eigen=False, 
                              parse_potcar_file=False, occu_tol=1e-08, exception_on_bad_xml=True)
            # getting total energy from VASP output
            total_energies_dict[q] = vasprun.final_energy
          
        for q in total_energies_dict:
            # creating vacancy object
            vacancy = Vacancy(structure_pure,defect_site,charge=q,multiplicity=1)
            energy_diff = total_energies_dict[q] - Epure
            
            if input_frey != None:
                # getting Freysold correction from input folder
                corr_file = input_frey + '/' + vacancy_type + '-vacancy/' + f'Charged{q}/freysold_corr_output'
                # checking if file exists
                if os.path.isfile(corr_file):
                    freysold_corr = float(grep(corr_file,'Defect correction')[0].split()[3])
                else:
                    freysold_corr = 0
                corrections = {'freysold':freysold_corr}
            else:
                corrections=None
            # creating Defect entry and adding it to entry list
            defect_entry = DefectEntry(vacancy,energy_diff, corrections=corrections, parameters=None, entry_id= f'{vacancy_type}_q{q}' )
            defect_entries.append(defect_entry)

# create defect phase diagram        
defect_pd = DefectPhaseDiagram(defect_entries,vbm,band_gap)

# getting chemical potential dictionary with format {Element(pymatgen object):chem_potential}
mu_elts = {}
for res in chempot:
    mu_elts[res] = {}
    for el in chempot[res]:
        element = Element(el)
        # creating dictionary of chemical potentials to be read
        mu_elts[res][element] = chempot[res][el] + chempot_ref[el]
        
# ANALYSIS ##########################################################################################
# calculating defect concentrations and fermi level for different reservoirs
concentrations = {}
fermi_level = {}

# getting bulk DOS - also unit cell DOS works
bulk_dos = vasprun_pure.complete_dos
    
for res in chempot:    
                
    # getting concentration of defects    
    concentrations[res] = defect_pd.defect_concentrations(mu_elts[res],temperature=300,fermi_level=0)    
    
    # getting fermi level position for specific reservoir 
    fermi_level[res] = defect_pd.solve_for_fermi_energy(300,mu_elts[res],bulk_dos)
    print(f'Fermi energy in reservoir "{res}" = {fermi_level[res]}')

######################################################################################################
 ## PLOTTING
if get_plots:  
    print('Getting plots...\n')      
    for res in chempot:
        # the 'size' argument has been added to 'plot' function    
        defect_pd.plot(mu_elts=mu_elts[res], xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1,
             lg_position=None, fermi_level=None, title=res, saved=False,size = 1.05)
    
        plt.savefig(f'plot_{system_name}_{res}.pdf')
        
        
        
        
        
        
        
