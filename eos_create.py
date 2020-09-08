#!/nfshome/villa/anaconda3/bin/python

import os
from shutil import copyfile

from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints
from pymatgen.core.structure import Structure, SiteCollection
from pymatgen.core.sites import Site
from pymatgen.core.lattice import Lattice

print('')
print('Script to create input files for EOS calculations')
print('')

# set up common INCAR - dictionary
incar_dict = {
    "KPAR": 4,
    "NPAR": 24,
    "NBANDS": 96,
    "ISTART": 1,
    "ICHARG": 1,
    "IBRION": 2,
    "NSW": 100,
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
   # "LVTOT": ".TRUE."        
   # "NEDOS": 2000,
    "LHFCALC" : ".TRUE.",
    "HFSCREEN": 0.2,
    "NKRED": 2,
    "PRECFOCK": "Fast",
    "AEXX": 0.24,
 }     


# IMPORT UNIT CELL POSCAR AND CREATE POSCAR OF SUPERCELL
# reading vasprun.xml and CONTCAR with pymatgen
unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure
struct_dict = struct.as_dict()
lattice_par = struct_dict['lattice']['a']
print(f'Original lattice parameter = {lattice_par} A')
print('')

# range in whiich lattice parameter is changed
lattice_range = lattice_par * 0.05
a_min = lattice_par - lattice_range - lattice_range/5 #starting point is on the right of the minimum
a_max = lattice_par + lattice_range
# n° of lattice parameter values
n_values = 30
interval = (a_max - a_min)/n_values

a = a_min

for i in range(1, n_values+1 , 1):
    
     # calculations directory
     calc_dir = './'
     # setting up destination directory     
     file_dir = calc_dir + 'a_' + str(i) + '/'
    # checking if path already exists - if not create it
     if not os.path.exists(file_dir):
            os.makedirs(file_dir)
     else:
         print(f'{file_dir} already exists')
                
            
# INCAR     
     # creating Incar object
     incar = Incar(incar_dict)
     # writing INCAR in designated path
     incar.write_file(file_dir + 'INCAR')
 
# POSCAR
    
     a = a_min + interval*i 
     
     new_lattice = Lattice([[a,0,0],[0,a,0],[0,0,a]])
     # creating new structure
     struct.lattice = new_lattice     
#     new_struct_dict = struct.as_dict()
#     print(new_struct_dict['lattice']['a'])
     poscar = Poscar(struct)      
     poscar.write_file(file_dir + 'POSCAR')        
     
# KPOINTS     
     
     k = 6
     kpoints = Kpoints.gamma_automatic(
         kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
         ) 
     
     kpoints.write_file(file_dir + 'KPOINTS')
     
# POTCAR     
     copyfile('./POTCAR', file_dir + 'POTCAR')
     
     
# job_vasp.sh
     
          # setting up job_vasp.sh script
     new_job_vasp_file = open(file_dir + 'job_vasp.sh','w')
     
     import re
     with open('./job_vasp.sh') as origin_file:
           # searching lines in input file   
           for line in origin_file:
              # emulating 'grep' command
              job_line = re.findall(r'#SBATCH --job-name=', line)
              # if line is the job line print customized job name                      
              if job_line:
                  # writing line with new job name
                  new_job_vasp_file.write(f'#SBATCH --job-name=NN_bm_2_{i} \n')
              else:
                  # if line doesn't contain job-name write same line from reference job_vasp.sh
                  new_job_vasp_file.write(line)                             
           new_job_vasp_file.close()      
         
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
