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
import copy
import matplotlib
import matplotlib.pyplot as plt

from pymatgen.core.periodic_table import Element
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.corrections import FreysoldtCorrection
from pymatgen.analysis.defects.thermodynamics import DefectPhaseDiagram
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.vasp.outputs import Vasprun, Locpot, VolumetricData
from pymatgen.electronic_structure.dos import CompleteDos

from my_functions.grep import grep
from my_functions.get_freysoldt_correction import get_freysoldt_correction
############################################################################
print('')
print('')
print('Usage : pmg_E-form_vacancies.py.py   path_to_def_calc_folder \n')

print('Please give delta chemical potentials in the different reservoirs in "chempot" dictionary\n')
print('Please give reference chemical potentials in the "chempot_ref" dictionary\n')
print('Please give path of pure calculation (unit cell or supercell both work) in the script\n')
print('Be careful that the naming of the folders is correct :')
print('     .../defect_system/{element}-vacancy/Charged{charge}/{scheme}/vasp_data \n')
print('')  
print('')

#############################################################################
############ INPUT ##########################################################
#############################################################################
'''CHEMICAL POTENTIALS'''
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

# dielectric constant
dielectric_constant = 11.95

'''CALCULATION OF PURE TOTAL ENERGY AND DENSITY OF STATES '''
# path of pure DOS and total energy calculation - can be of unit cell or supercell
# the path for the LOCPOT file will be searched under 'Pure' folder in the system folder 
# this is because the LOCPOT file needs to have the supercell structure and cannot be a unit cell calculation 
pure_path = '/nfshome/villa/local-data/NN_cubic/DOS-BS/PBE/1-DOS' + '/'

'''CALCULATION SCHEME'''
# usually fixed but it's possible to change it - paths for LOCPOT and vasprun.xml files for PBE and HSE schemes
calc_scheme_types = {'PBE':{'locpot':'1-PBE-SCF',
                            'vasprun':'2-PBE-OPT'},
                     'HSE':{'locpot':'6-HSE-SCF',
                            'vasprun':'7-HSE-OPT'}
                      }
# select your current calculation scheme
calc_scheme = calc_scheme_types['PBE']

'''CORRECTIONS'''
# dictionary for corrections to include in DefectEntry
corrections = {}
# include Freysoldt corrections
include_freysoldt_corrections = True 

'''PLOTTING'''
# getting and saving plots
get_plots = True
# get subplot with different reservoirs - get_plots needs to be set to True
get_subplot = True
# subplot settings - number of rows and columns of subplot
nrows,ncolumns = 2,3

###############################################################################
###############################################################################


print(f'Path of pure calculation: "{pure_path}"\n')

# folder with defects calculations
#input_path = sys.argv[1] + '/'
input_path = '/nfshome/villa/local-data/NN_cubic/vacancies-PBE' + '/'
system_name = os.path.basename(os.path.dirname(input_path))

# getting pure structure from LOCPOT file of pure supercell calculation
path_to_pure_locpot = input_path + '/Pure/'+ calc_scheme['locpot'] +'/LOCPOT'
structure_pure = Locpot.from_file(path_to_pure_locpot).structure
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
# saving initial corrections dictionary
corrections_init = corrections.copy() 

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
            vasprun = Vasprun(charge_dir + calc_scheme['vasprun']+'/vasprun.xml',ionic_step_skip=None, ionic_step_offset=0,
                              parse_dos=False, parse_eigen=False, parse_projected_eigen=False, 
                              parse_potcar_file=False, occu_tol=1e-08, exception_on_bad_xml=True)
            # getting total energy from VASP output
            total_energies_dict[q] = vasprun.final_energy
          
        for q in total_energies_dict:
            # creating vacancy object
            vacancy = Vacancy(structure_pure,defect_site,charge=q,multiplicity=1)
            energy_diff = total_energies_dict[q] - Epure
            
            corrections = corrections_init.copy()
            
            # if requested calculate freysoldt corrections
            if include_freysoldt_corrections:
                path_to_defect_locpot = charge_dir + calc_scheme['locpot'] +'/LOCPOT'
                path_to_pure_locpot = input_path + '/Pure/'+ calc_scheme['locpot'] +'/LOCPOT'
                freysoldt_corrections = get_freysoldt_correction(path_to_defect_locpot,path_to_pure_locpot,
                                                                 q, dielectric_constant, defect_site_coord,
                                                                 energy_cutoff = 500)
                freysoldt_correction_total = (freysoldt_corrections['freysoldt_electrostatic'] + 
                                              freysoldt_corrections['freysoldt_potential_alignment'])
                print(f'Freysoldt correction for V_{vacancy_type} ({q}) = {freysoldt_correction_total} eV')
            else:
                freysoldt_correction_total = 0
                    
            corrections['freysoldt_correction'] = freysoldt_correction_total   
            
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
    
    if get_subplot==True:
        print('Getting subplot...\n')
        sub_index = 0
        plt.figure(figsize=(30,16))
        matplotlib.rc('xtick', labelsize=20) 
        matplotlib.rc('ytick', labelsize=20)
        for res in chempot:
            sub_index += 1
            subplot_settings = [nrows,ncolumns,sub_index] # list of numbers for subplot setting - plt.subplot(nrows,nclumns,index)
            # the 'size' argument has been added to 'plot' function    
            defect_pd.plot(mu_elts=mu_elts[res], xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1.7,
                 lg_position=None, fermi_level=None, title=res, saved=False,
                 size = 1,get_subplot=get_subplot,subplot_settings=subplot_settings)
    
        plt.savefig(f'plot_{system_name}.pdf')
        
    else:
        print('Getting plots...\n')
        for res in chempot:
            # the 'size' argument has been added to 'plot' function    
            defect_pd.plot(mu_elts=mu_elts[res], xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1.7,
                 lg_position=None, fermi_level=None, title=res, saved=False,size = 1.05)
    
            plt.savefig(f'plot_{system_name}_{res}.pdf')        
        


# Getting concentration of defects and Fermi level at different temperatures

conc_file = open(f'concentration_{system_name}.dat','w')
conc_file.write(f'# {system_name} \n')
conc_file.write('# Temperature (K)   VO+2   VNa-1 \n')
                
res = 'E'                
        
fig ,ax = plt.subplots(figsize=(10,8))        
for T in range(1,1000,50):
    concentration = defect_pd.defect_concentrations(mu_elts[res],temperature=T,fermi_level=fermi_level[res])
    
    VO_p2 = concentration[10]
    VNa_m1 = concentration[3]
    
    conc_file.write('%i %e %e\n' %(T, VO_p2['conc'], VNa_m1['conc']))

conc_file.close()    

  
fermi_file = open(f'fermi-level_{system_name}.dat','w')
fermi_file.write(f'# {system_name} \n')
fermi_file.write('# Temperature (K)  Fermi level \n')
                
for T in range(1,1000,50):    
    fermi = defect_pd.solve_for_fermi_energy(T,mu_elts['E'],bulk_dos)
    fermi_file.write('%f %f \n' %(T , fermi))
    
fermi_file.close()

