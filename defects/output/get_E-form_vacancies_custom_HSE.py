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
from pymatgen.core.composition import Composition
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.corrections import FreysoldtCorrection
from pymatgen.analysis.defects.thermodynamics import DefectPhaseDiagram
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.vasp.outputs import Vasprun, Locpot, VolumetricData
from pymatgen.electronic_structure.dos import CompleteDos

from my_functions.grep import grep
from my_functions.get_freysoldt_correction import get_freysoldt_correction
from my_functions.defects import SingleDefectData, DefectsAnalysis
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
chempot =   {'A':{'Na':-0.5,
                  'Nb':-2.52,
                  'O':-3.54},
             'B':{'Na':-1.69,
                  'Nb':-1.56,
                  'O':-3.47},
             'C':{'Na':-3.43,
                  'Nb':-10.22,
                  'O':  0},
             'D':{'Na':-1.85,
                  'Nb':-9.25,
                  'O':  -0.85},
             'E':{'Na':-1.8,
                  'Nb':-6,
                  'O':-1.95}
              }
# reference chemical potentials
chempot_ref = {'Na':-1.3840,
               'Nb':-11.6471,
               'O': -12.9962/2}

# dielectric constant
dielectric_constant = 4.6904 # only electronic part (LOPTICS) because LEPSILON doesn't work for HSE

'''CALCULATION OF PURE TOTAL ENERGY AND DENSITY OF STATES '''
# path of pure DOS and total energy calculation - can be of unit cell or supercell
# the path for the LOCPOT file will be searched under 'Pure' folder in the system folder 
# this is because the LOCPOT file needs to have the supercell structure and cannot be a unit cell calculation 
pure_path = '/nfshome/villa/local-data/NN_cubic/vacancies-HSE-new-method/Pure' + '/'

'''CALCULATION SCHEME'''
# usually fixed but it's possible to change it - paths for LOCPOT and vasprun.xml files for PBE and HSE schemes
calc_scheme_types = {'PBE':{'locpot':'1-PBE-SCF',
                            'vasprun':'2-PBE-OPT'},
                     'HSE':{'locpot':'6-HSE-SCF',
                            'vasprun':'7-HSE-OPT'}
                      }
# select your current calculation scheme
calc_scheme = calc_scheme_types['HSE']

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
input_path = '/nfshome/villa/local-data/NN_cubic/vacancies-HSE-new-method' + '/'
system_name = os.path.basename(os.path.dirname(input_path))

# getting pure structure from LOCPOT file of pure supercell calculation
path_to_pure_locpot = input_path + 'Pure/' +'LOCPOT'
structure_pure = Locpot.from_file(path_to_pure_locpot).structure
# n° of atoms in supercell structure
natoms_sup = len(structure_pure.sites)

# getting values of pure structure
vasprun_pure = Vasprun(pure_path + 'vasprun.xml')
# getting band_gap and vbm
(band_gap,cbm, vbm, is_band_gap_direct) = vasprun_pure.eigenvalue_band_properties
# n° atoms in pure structure calculation
natoms_pure = len(Locpot.from_file(pure_path+'LOCPOT').structure.sites)
Epure = ((vasprun_pure.final_energy)/natoms_pure)*natoms_sup
# creating a list of directories contained in input directory
list_dir_vacancies = glob(input_path + '*/')

# saving initial corrections dictionary
corrections_init = corrections.copy() 

# list with defects
defect_list = []

for vacancy_dir in list_dir_vacancies:
    if '-vacancy' in vacancy_dir:
        vacancy_type = os.path.basename(os.path.dirname(vacancy_dir)).replace('-vacancy','')
        if  True: #vacancy_type == 'Na':
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
                
                if os.path.isfile(charge_dir + calc_scheme['vasprun']+'/vasprun.xml') :
                    # reading vasprun.xml file
                    vasprun = Vasprun(charge_dir + calc_scheme['vasprun']+'/vasprun.xml',ionic_step_skip=None, ionic_step_offset=0,
                                      parse_dos=False, parse_eigen=False, parse_projected_eigen=False, 
                                      parse_potcar_file=False, occu_tol=1e-08, exception_on_bad_xml=True)
                    # getting total energy from VASP output
                    total_energies_dict[q] = vasprun.final_energy
              
            for q in total_energies_dict:
                # creating vacancy object
                vacancy = Vacancy(structure_pure,defect_site,charge=q,multiplicity=1)
                
                # including corrections
                corrections = corrections_init.copy()
                
                # if requested calculate freysoldt corrections
                if include_freysoldt_corrections:
                    path_to_defect_locpot = charge_dir + calc_scheme['locpot'] +'/LOCPOT'
                    path_to_pure_locpot = input_path + 'Pure'+ '/LOCPOT'
                    freysoldt_corrections = get_freysoldt_correction(path_to_defect_locpot,path_to_pure_locpot,
                                                                     q, dielectric_constant, defect_site_coord,
                                                                     energy_cutoff = 500)
                    freysoldt_correction_total = (freysoldt_corrections['freysoldt_electrostatic'] + 
                                                  freysoldt_corrections['freysoldt_potential_alignment'])
                    print(f'Freysoldt correction for V_{vacancy_type} ({q}) = {freysoldt_correction_total} eV')
                else:
                    freysoldt_correction_total = 0
                        
                corrections['freysoldt_correction'] = freysoldt_correction_total
                
                delta_atoms = {Element(vacancy_type):-1}
                energy_diff = total_energies_dict[q] - Epure
                single_defect_data = SingleDefectData(structure_pure,delta_atoms,energy_diff,corrections,charge=q,name=f'Vac_{vacancy_type}')
                
                # append in defect list
                defect_list.append(single_defect_data)



defects_analysis = DefectsAnalysis(defect_list,vbm,band_gap)

# getting chemical potential dictionary with format {Element(pymatgen object):chem_potential}
mu_elts = {}
for res in chempot:
    mu_elts[res] = {}
    for el in chempot[res]:
        element = Element(el)
        # creating dictionary of chemical potentials to be read
        mu_elts[res][element] = chempot[res][el] + chempot_ref[el]


#   PLOTTING #
if get_plots:
    if get_subplot:
        index = 0
        for res in mu_elts:
            index += 1
            defects_analysis.plot(mu_elts=mu_elts[res],ylim = (-5,20), title = res,plotsize = 2,
                                  format_legend=True,get_subplot=get_subplot,subplot_settings=[nrows,ncolumns,index])
   #     plt.show()

    else:
        for res in mu_elts:
            defects_analysis.plot(mu_elts=mu_elts[res],ylim = (-5,20), title = res,plotsize = 1.2,
                                  format_legend=True,get_subplot=False)

plt.savefig('vacancies_HSE.pdf')

vasprun_dos = '/nfshome/villa/local-data/NN_cubic/vacancies-HSE-new-method/Pure-unit-cell/vasprun.xml'
bulk_dos = Vasprun(vasprun_dos).complete_dos

for res in mu_elts:
    fermi_level = defects_analysis.equilibrium_fermi_level(mu_elts[res], bulk_dos, temperature = 300)
    print(f'Fermi level in reservoir {res} = {fermi_level}')
#
#defects_analysis.plot(mu_elts=mu_elts['D'],ylim=(-5,20),title='D',plotsize=1,format_legend=True)
#plt.savefig('vacancies_HSE_Nb_res_D.pdf')
#plt.savefig('vacancies_HSE_Nb_res_D.png')
        
        
    
    
    
    
    
    
    
    
    
