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

############################################################################
print('')
print('')
print('Usage : Ef_plot_defects_corr.py   path_to_def_calc_folder')
print('')
print('Please give Energy of Pure phase, VBM , band gap and path to pure calculation in the script')
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
# values of pure structure
Epure = -1032.98698590
vbm =  1.5365 # eV
band_gap = 1.527

# path of pure DOS calculation
pure_path = '/nfshome/villa/local-data/NN_cubic/DOS-BS/PBE/1-DOS' + '/'
# getting and saving plots
get_plots = True 

###############################################################################
###############################################################################

# folder with defects calculations
input_path = sys.argv[1] + '/'
#input_path = '/nfshome/villa/local-data/NN_cubic/vacancies-PBE' + '/'
system_name = os.path.basename(os.path.dirname(input_path))
# folder with Freysold corrections
#input_frey = sys.argv[2] + '/'

# getting pure structure
structure_pure = Poscar.from_file(input_path+"POSCAR_supercell").structure

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
            # creating Defect entry and adding it to entry list
            defect_entry = DefectEntry(vacancy,energy_diff, corrections=None, parameters=None, entry_id= f'{vacancy_type}_q{q}' )
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
    
for res in chempot:    
                
    # getting concentration of defects    
    concentrations[res] = defect_pd.defect_concentrations(mu_elts[res],temperature=300,fermi_level=0)
    
    # getting bulk DOS - also unit cell DOS works
    vasprun_unit = pure_path + 'vasprun.xml'
    vasprun = Vasprun(vasprun_unit)
    bulk_dos = vasprun.complete_dos
    
    # getting fermi level position for specific reservoir 
    fermi_level[res] = defect_pd.solve_for_fermi_energy(300,mu_elts[res],bulk_dos)
    print(f'Fermi energy in reservoir "{res}" = {fermi_level[res]}')

######################################################################################################
 ## PLOTTING
if get_plots:        
    for res in chempot:
        # the 'size' argument has been added to 'plot' function    
        defect_pd.plot(mu_elts=mu_elts[res], xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1,
             lg_position=None, fermi_level=None, title=res, saved=False,size = 1.05)
    
        plt.savefig(f'plot_{system_name}_{res}.pdf')
        
        
        
        
        
        
        
