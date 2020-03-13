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

from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints
from pymatgen.core.structure import Structure, SiteCollection
from pymatgen.core.sites import Site

##############################################################################
print('')
print('''Script that sets up files for vacancy calculations:
    Pure structure :
    - step 1 - electronic SCF with ENCUT = 500 eV and EDIFF = 1e-06 eV
    - step 2 - relaxation of atomic positions with ISIF = 2 and EDIFFG = 0.05 eV/A
    - step 3 - volume relaxation (ISIF = 3) 
    - step 4 - calculation with LOPTICS = .TRUE. to obtain dielectric constant 
    Vacancy :
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
print('''You must set up the desired CHARGE STATES for each vacancy type 
in the dictionary at the beginning of the script''')    
print('')


# system name to be inserted in job_vasp.sh
job_system_name = 'N'

###########################################################################
# SETTING CHARGE STATE DICTIONARY
charge_states_dict = {'Na':[-1,0,1],
                      'Nb':[-5,-4,-3,-2,-1,0,1],
                      'O':[-2,-1,-0,1,2]}
###########################################################################

# names for folders of pure structure
step_pure_names = ['1-PBE-SCF/','2-PBE-OPT/','3-PBE-VOPT/']
# names for folders of pure structure
step_def_names = ['1-PBE-SCF/','2-PBE-OPT/']

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
    "LVTOT": ".TRUE."        
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

for s in range(2,5):

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
        
    
#    # supercell size    
#    s = 3    
    # K-mesh k x k x k
    if s == 2:
        k = 3
    else:
        k = 2
    # CREATING SUPERCELL 
    struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
    struct_dict = struct.as_dict()
    poscar = Poscar(struct)
    s_cell_path = f'./{s}-supercell/'
    if not os.path.exists(s_cell_path):
        os.makedirs(s_cell_path) 
    
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
    pure_path = s_cell_path + '/Pure/'
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
    for st in step_pure_names:
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
         # creating customized job_vasp.sh file for every step     
        new_job_vasp_file = open(pure_path + step + 'job_vasp.sh','w')
        with open('./job_vasp.sh') as origin_file:
              # searching lines in input file   
               for line in origin_file:
                 # emulating 'grep' command
                  job_line = re.findall(r'#SBATCH --job-name=', line)
                  # if line is the job line print customized job name                      
                  if job_line:
                      # WRITING NEW LINE WITH NEW JOB NAME
                      step_number = step_pure_names.index(step) + 1
                      new_job_vasp_file.write(f'#SBATCH --job-name=N_{s}P_{step_number} \n')
                  else:
                      # if line doesn't contain job-name write same line from reference job_vasp.sh
                      new_job_vasp_file.write(line)                             
               new_job_vasp_file.close()
          
    #############################################################################         
    #############################################################################
    
    
    #############################################################################
    #   CYCLE OVER DIFFERENT TIPES OF VACANCIES
    #############################################################################
    
            
    # save original structure
    pure_struct = struct.copy()
    #initializing  index of atom to be removed - set to -1 so it will pick the last {el} atom in POSCAR
    rem_atom_index = -1
    
    
    for el in type_elements:
      if el == 'O':  
         # reset original structure
         struct = pure_struct.copy()     
         # reset flag for atom removal
         remove_atom = False  
         # setting path
         element_path =  (s_cell_path + f'/{el}-vacancy/')
        # checking if path already exists - if not create it
         if not os.path.exists(element_path):
                os.makedirs(element_path)
              
         #opening info file
         info_file = open(element_path + 'info', 'w')   
         
         # iteration over charge states
         for q in charge_states_dict[el]:
             
             # reset initial INCAR
             incar_dict = init_incar_dict.copy()
                      
             charge_path = element_path + f'Charged{q}/'         
             if not os.path.exists(charge_path):
                os.makedirs(charge_path)
             else:
                print(f'Directory {charge_path} already exists')
             
             for step in step_def_names:   
                 if not os.path.exists(charge_path + step):   
                    os.makedirs(charge_path + step)
                
             #####################################################################  
             # set up POSCAR
             # if POSCAR hasn't been created yet remove one atom and create POSCAR
             if remove_atom == False:
                 # atom index to remove -> last atom in original POSCAR
                 rem_atom_index += elements[el] 
                 # finding coordinates of atom to remove(only to print them)
                 rem_atom_coord = struct_dict['sites'][rem_atom_index]['abc']
                 # printing coordinates of removed atom
                 info_file.write(f'Coordinates of removed atom for {el} vacancy : \n')
                 info_file.write('%f,%f,%f' %(rem_atom_coord[0], rem_atom_coord[1], rem_atom_coord[2]))
                 info_file.close()
                 # remove atom 
                 struct.remove_sites([rem_atom_index])
                 # create POSCAR
                 poscar_def = Poscar(struct)
                 remove_atom = True         
             #writing POSCARs
             for step in step_def_names:
                 poscar_def.write_file(charge_path + step + 'POSCAR')
            
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
                     nelect += (elements[element]-1)*val_el_dict[element]
                 else:
                     nelect += elements[element]*val_el_dict[element] 
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
             # copy POTCAR in all directories
             for step in step_def_names:
                 copyfile('./POTCAR', charge_path + step + 'POTCAR')
            
             #####################################################################
             # creating customized job_vasp.sh file
    
             for step in step_def_names:
                     # creating customized job_vasp.sh file for every step      
                    new_job_vasp_file = open(charge_path + step + 'job_vasp.sh','w')
                    with open('./job_vasp.sh') as origin_file:
                          # searching lines in input file   
                           for line in origin_file:
                             # emulating 'grep' command
                              job_line = re.findall(r'#SBATCH --job-name=', line)
                              # if line is the job line print customized job name                      
                              if job_line:
                                  # WRITING NEW LINE WITH NEW JOB NAME
                                  step_number = step_def_names.index(step) + 1
                                  new_job_vasp_file.write(f'#SBATCH --job-name=N_{s}{el}{q}_{step_number} \n')
                              else:
                                  # if line doesn't contain job-name write same line from reference job_vasp.sh
                                  new_job_vasp_file.write(line)                             
                           new_job_vasp_file.close()
                   
              ####################################################################    
