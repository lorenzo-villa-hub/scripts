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

from pymatgen.core.periodic_table import Element
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.thermodynamics import DefectPhaseDiagram
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.vasp.outputs import Vasprun

############################################################################
print('')
print('')
print('Usage : Ef_plot_defects_corr.py   path_to_def_calc_folder   path_to_freysold_corr_folder')
print('')
print('')
print('PLEASE GIVE PATH OF DATA AS INPUT')
print('')
print('PLEASE GIVE VALUES OF Etot_pure, vbm AND chem potential IN THE SCRIPT')  

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
Epure = -799.45169854
vbm =  1.6175 # eV
band_gap = 3.2

###############################################################################
###############################################################################

# folder with defects calculations
#input_path = sys.argv[1] + '/'
input_path = '/nfshome/villa/local-data/NN_cubic/vacancies-PBE+U11/O-vacancy/'
system_name = os.path.basename(os.path.dirname(input_path))
common_path = os.path.split(os.path.dirname(input_path))[0]+ '/'
# folder with Freysold corrections
#input_frey = sys.argv[2] + '/'

# getting pure structure
structure_pure = Poscar.from_file(common_path+"POSCAR_supercell").structure
# reading info file
with open(input_path+'info','r') as info_file:
    coord = info_file.readlines()[1].split(',')
    defect_site_coord = np.asarray(coord,dtype = np.float64)
    
# finding defect site
for site in structure_pure.sites:
    if np.array_equiv(site.frac_coords,defect_site_coord):
        defect_site = site
        
# dictionary of total energies        
total_energies_dict = {}

# creating a list of directories contained in input directory
list_dir = glob(input_path + '*/')
# search in every directory of the list
for dir in list_dir:
    # finding charge state from subfolder name
    q = int(os.path.basename(os.path.dirname(dir)).replace('Charged',''))
    # build path of vasprun.xml file
    vasprun = Vasprun(dir + '2-PBE-OPT/vasprun.xml',ionic_step_skip=None, ionic_step_offset=0,
                      parse_dos=False, parse_eigen=False, parse_projected_eigen=False, 
                      parse_potcar_file=False, occu_tol=1e-08, exception_on_bad_xml=True)
    # getting total energy from VASP output
    total_energies_dict[q] = vasprun.final_energy

# creating defect entries
defect_entries = []    
for q in total_energies_dict:
    vacancy = Vacancy(structure_pure,defect_site,charge=q)
    energy_diff = total_energies_dict[q] - Epure
    defect_entry = DefectEntry(vacancy,energy_diff, corrections=None, parameters=None, entry_id= f'q{q}' )
    defect_entries.append(defect_entry)

defect_pd = DefectPhaseDiagram(defect_entries,vbm,band_gap)

for res in chempot:
    mu_elts = {}
    for el in chempot[res]:
        element = Element(el)
        mu_elts[element] = chempot[res][el] + chempot_ref[el]
        
    plt = defect_pd.plot(mu_elts=mu_elts, xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1.0,
         lg_position=None, fermi_level=None, title=res, saved=False)
    plt.show()
        
    


        
