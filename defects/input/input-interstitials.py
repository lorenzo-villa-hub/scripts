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

from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints, Potcar
from pymatgen.core.structure import Structure, SiteCollection
from pymatgen.core.sites import Site

##############################################################################
print('')
print('''Script that sets up files for interstitials calculations:
    Pure structure :
    - step 1 - electronic SCF with ENCUT = 500 eV and EDIFF = 1e-06 eV
    - step 2 - relaxation of atomic positions with ISIF = 2 and EDIFFG = 0.05 eV/A
    - step 3 - volume relaxation (ISIF = 3) 
    - step 4 - calculation with LOPTICS = .TRUE. to obtain dielectric constant 
    Interstitial :
    - step 1 - electronic SCF with ENCUT = 500 eV and EDIFF = 1e-06 eV
    - step 2 - relaxation of atomic positions with ISIF = 2 and EDIFFG = 0.05 eV/A''')
print('')    
print(' WARNING !!!')
print('')
print('''Requires 3 files to be present in working directory:
    - The POSCAR file of the unit cell named "POSCAR_unit"
    - A standard submission file for Lichtenberg named "job_vasp.sh"
    - POTCAR file ''')
print('')    
print('''You must set up the desired CHARGE STATES for each interstitial type 
in the dictionary at the beginning of the script''')    
print('')



''' INPUT BEGIN'''

# system name to be inserted in job_vasp.sh
job_system_name = 'N'

###########################################################################
# SETTING CHARGE STATE DICTIONARY
charge_states_dict = {'Na':[0,1],
                      'Nb':[5,4,3,2,1,0,-1],
                      'O':[-2,-1,-0,1,2]}
###########################################################################

# U parameters dictionay
U_parameters = {'Na':0,
                'Nb':11,
                'O':0}
###########################################################################

# POTCAR names
potcar_choices = {'Na':'Na',
                  'Nb':'Nb_pv',
                  'O':'O'}

# names for folders of pure structure
step_pure_names = ['1-PBE-SCF/','2-PBE-OPT/','3-PBE-VOPT/','4-PBE-DC/']
# names for folders of pure structure
step_def_names = ['1-PBE-SCF/','2-PBE-OPT/']

'''INPUT END '''


##############################################################################
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

###############################################################################
# function to create interstitials
# might take long
def create_interstitial_supercells(structure,element,size=2):
    
    '''return a dictionary with sites (Site object) and supercell
       structures (Structure object) for interstitials of a given pure 
       structure (Structure object) '''
    
    from pymatgen.analysis.defects.utils import StructureMotifInterstitial
    
    # get interstitials with InFiT (Interstitialcy Finding Tool) algorithm - check pymatgen for details
    int_object = StructureMotifInterstitial(structure,element)
    int_sites = int_object.enumerate_defectsites()
    int_supercells = int_object.make_supercells_with_defects([[size,0,0],[0,size,0],[0,0,size]])
    
    interstitials = {}
    interstitial_sites =  {}
    interstitial_structures = {}
    # getting properties and supercells for all interstitial sites
    for i in range(0,len(int_sites)):
        
        int_coord_type = int_object.get_motif_type(i)
        interstitial_sites[int_coord_type] = int_sites[i]
        # first supercell in list is the pure
        struct_int = int_supercells[i+1]
        interstitial_structures[int_coord_type] = struct_int
        
        interstitials['sites'] = interstitial_sites
        interstitials['structures'] = interstitial_structures
        
    return interstitials
##############################################################################
##############################################################################
    

##############################################################################
# set up common INCAR - dictionary
incar_dict = {
    "KPAR": 4,
    "NPAR": 24,
    "NBANDS": 480,
    "ISTART": 0,
    "ICHARG": 2,
    "IBRION": 2,
    "NSW": 0,
    "ISIF": 2,
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
    "LVTOT": ".TRUE.",

    "NEDOS": 2000,
    "LDAU" : ".TRUE.",
    "LDAUU": f"{U_parameters['Na']} {U_parameters['Nb']} {U_parameters['O']}",
    "LDAUTYPE": 2,
    "LDAUPRINT": 2,
        
   # "NEDOS": 2000,
   # "LHFCALC" : ".TRUE.",
   # "HFSCREEN": 0.2,
   # "NKRED": 2,
   # "PRECFOCK": "Fast",
   # "AEXX": HF,
 }     
#############################################################################

##########################################################################

# initial INCAR - without copy function the 2 dict are linked
init_incar_dict = incar_dict.copy()


# IMPORT UNIT CELL POSCAR AND CREATE POSCAR OF SUPERCELL
# reading vasprun.xml and CONTCAR with pymatgen
unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure

# getting dict of elements in POSCAR
number_elements = unit_poscar.natoms
type_elements = unit_poscar.site_symbols
elements = {}
for i in type_elements:
    index_list = type_elements.index(i)
    elements.update({i : number_elements[index_list]})
    

##############################################################################
# MAKING DICTIONARY WITH INTERSTITIALS SUPERCELLS
interstitial_dict = {}

for el in type_elements:
    
    interstitial_dict[el] = create_interstitial_supercells(struct,el,size = 3)
##############################################################################
  

# supercell size    
s = 3    
# K-mesh k x k x k
k = 2
# CREATING SUPERCELL 
struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
#struct_dict = struct.as_dict()
poscar = Poscar(struct)
poscar.write_file("POSCAR_supercell")

# getting dict of elements in supercell POSCAR
number_elements = poscar.natoms
type_elements = poscar.site_symbols
elements = {}
for i in type_elements:
    index_list = type_elements.index(i)
    elements.update({i : number_elements[index_list]})
    
# get number of electrons for each element in POSCAR
# valence electron dictonary works the same way as elements dictonary
val_el_dict = {}
# list of valence electrons to create valence_el dictonary
val_el_list = []

with open("POTCAR") as origin_file:
# searching lines in input file   
  for line in origin_file:
    # emulating 'grep' command
    zval_line = re.findall(r'ZVAL', line)
    if zval_line:
    # splits line  
       zval_line = line.split()
       # valence electron number is the 5th character
       val_el_list.append(float(zval_line[5]))
# write valence electrons dictonary
counter = 0       
for el in type_elements:
    val_el_dict[el] = val_el_list[counter]
    counter += 1


##########################################################################
##########################################################################
# PURE STRUCTURE 
# set up calculation of Pure structure
pure_path = './Pure/'
if not os.path.exists(pure_path):
    os.makedirs(pure_path)
else:
    print(f'Directory {pure_path} already exists') 
for step in step_pure_names:          
    if not os.path.exists(pure_path + step):   
        os.makedirs(pure_path + step)
    

#########################################################################
# INCAR for step 1   
incar = Incar(incar_dict)
incar.write_file(pure_path + step_pure_names[0] + 'INCAR')

if len(step_pure_names) > 1:
 
     # changing INCAR for step 2
    incar_dict['ISTART'] = 1
    incar_dict['ICHARG'] = 1
    incar_dict['EDIFF'] = 1e-05
    incar_dict['NSW'] = 100
    #write INCAR for step 2
    incar = Incar(incar_dict)
    incar.write_file(pure_path + step_pure_names[1] + 'INCAR')

if len(step_pure_names) > 2:
    
    # changing INCAR for step 3
    incar_dict['ISIF'] = 3
    
    incar = Incar(incar_dict)
    incar.write_file(pure_path + step_pure_names[2] + 'INCAR')

if len(step_pure_names) > 3:

    # changing INCAR for step 4
    nbands = incar_dict['NBANDS']
    incar_dict['NBANDS'] = nbands * 3
    incar_dict['EDIFF'] = 1e-06
    incar_dict.update({'NSW': 0})
    incar_dict.update({'NEDOS': 2000})
    incar_dict.update({'LOPTICS': '.TRUE.'})
    
    incar = Incar(incar_dict)
    incar.write_file(pure_path + step_pure_names[3] + 'INCAR')

#####################################################################
# copy POTCAR in all directories
for step in step_pure_names:
    copyfile('./POTCAR', pure_path + step + 'POTCAR')

#####################################################################
# set up KPOINTS
# KPOINTS
kpoints = Kpoints.gamma_automatic(
kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
)
for step in step_pure_names:
    kpoints.write_file( pure_path + step + 'KPOINTS')
        
#####################################################################
# write POSCAR
for step in step_pure_names:
    poscar.write_file(pure_path + step + 'POSCAR')

#####################################################################
#####################################################################

 #####################################################################
# creating customized names for sub file

for step in step_pure_names:

    step_number = step_pure_names.index(step) + 1
    job_dir = pure_path + step
    write_job_vasp(output_dir = job_dir, job_name = f'{job_system_name}_P_{step_number}') 
 
      
#############################################################################         
#############################################################################


#############################################################################
#   CYCLE OVER DIFFERENT TIPES OF INTERSTITIALS
#############################################################################

        
# save original structure
pure_struct = struct.copy()
#initializing  index of atom to be removed - set to -1 so it will pick the last {el} atom in POSCAR
int_atom_index = -1


for el in type_elements:

     # setting path
     element_path =  (f'./{el}-interstitial/')
    # checking if path already exists - if not create it
     if not os.path.exists(element_path):
            os.makedirs(element_path)
     
     # cycle over different types of coordination   
     for coord in interstitial_dict[el]['sites']:
         
         if coord == 'octahedral':
             coord_short = 'oct'
         if coord == 'tetrahedral':
             coord_short = 'tet'
         
         interstitial_path = element_path + coord + '/'
         if not os.path.exists(interstitial_path):
                os.makedirs(interstitial_path)
        
         #writing coordinate of interstitial in info file
         info_file = open(interstitial_path + 'info', 'w')   
         int_atom_site = interstitial_dict[el]['sites'][coord].as_dict()
         int_atom_coord = int_atom_site['abc']
         # printing coordinates of removed atom
         info_file.write(f'#Coordinates of interstitial atom for {el} interstitial - {coord} coordination: \n')
         info_file.write('%f,%f,%f' %(int_atom_coord[0], int_atom_coord[1], int_atom_coord[2]))  
         info_file.close()
         # iteration over charge states
         for q in charge_states_dict[el]:
             
             # reset initial INCAR
             incar_dict = init_incar_dict.copy()
                      
             charge_path = interstitial_path + f'Charged{q}/'         
             if not os.path.exists(charge_path):
                os.makedirs(charge_path)
             else:
                print(f'Directory {charge_path} already exists')
             
             for step in step_def_names:   
                 if not os.path.exists(charge_path + step):   
                    os.makedirs(charge_path + step)
                
             #####################################################################  
             # set up POSCAR                    
             struct = interstitial_dict[el]['structures'][coord]
             struct.sort()
             
            # create POSCAR object
             poscar_def = Poscar(struct)
             elements_for_potcar = poscar_def.site_symbols                     
             #writing POSCARs
             for step in step_def_names:
                 poscar_def.write_file(charge_path + step + 'POSCAR')    
                 
             #####################################################################
             # copy POTCAR in all directories
             
             # set Potcar object
             potcar_symbols = []
             for item in elements_for_potcar:
                 potcar_symbols.append(potcar_choices[item])
                 
             potcar = Potcar(symbols=potcar_symbols, functional='PBE', sym_potcar_map=None)
            
             for step in step_def_names:
                 potcar.write_file(charge_path + step + 'POTCAR')
                            
             #####################################################################
             # set up KPOINTS
             # KPOINTS
             kpoints = Kpoints.gamma_automatic(
             kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
             )         
             # writing KPOINTs   
             for step in step_def_names:
                 kpoints.write_file(charge_path + step + 'KPOINTS')
             
             #####################################################################        
             # set up INCAR for step 1 and 2
             # calculate total n° of electrons accounting for charge state
             nelect = -1*q
             for element in elements:
                 if element == el:
                     # we have an interstitial : 1 atom more
                     nelect += (elements[element]+1)*val_el_dict[element]
                 else:
                     nelect += elements[element]*val_el_dict[element] 
             
             # write LDAUU tag with same order in POSCAR
             ldauu = ''
             for item in elements_for_potcar:
                 ldauu = ldauu + str(U_parameters[item]) + ' '
             incar_dict.update({'LDAUU':ldauu})
             #update NELECT in INCAR 
             incar_dict.update({'NELECT': nelect})
             # write INCAR for step 1
             incar = Incar(incar_dict)
             incar.write_file(charge_path + step_def_names[0] + 'INCAR')
             
             if len(step_def_names) > 1:
                 # changing INCAR for step 2
                 incar_dict['ISTART'] = 1
                 incar_dict['ICHARG'] = 1
                 incar_dict['EDIFF'] = 1e-05
                 incar_dict['NSW'] = 100
                 #write INCAR for step 2
                 incar = Incar(incar_dict)
                 incar.write_file(charge_path + step_def_names[1] + 'INCAR')
             

             #####################################################################
             # creating customized job_vasp.sh file
                        
             for step in step_def_names:
    
                  # WRITING NEW LINE WITH NEW JOB NAME
                  job_dir = charge_path + step
                  step_number = step_def_names.index(step) + 1
                  write_job_vasp(output_dir = job_dir,
                                 job_name = f'{job_system_name}_{el}_{coord_short}{q}_{step_number}')
          

              ####################################################################    
