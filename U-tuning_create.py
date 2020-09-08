# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 14:38:17 2019

@author: Lorenzo
"""

import os

#
# Pymatgen imports
#import pymatgen as mg
from pymatgen.io.vasp.inputs import Incar


# main body below
# U parameters varies from 2 to 8
for U in range (2,9,1):
# === Prepare Input ===
# INCAR
    
     incar_dict = {
        "KPAR": 4,
        "NPAR": 24,
        "ISTART": 1,
        "ICHARG": 1,
        "IBRION": 2,
        "NSW": 100,
        "ISIF": 3,
        "EDIFFG": -0.05,
        "NBANDS": 96,
        "ISPIN": 1,
        "LWAVE": ".TRUE.",
        "LCHARG": ".TRUE.",
        "LORBIT":11,
        "ENCUT": 500,
        "EDIFF": 1e-06,
        "ISMEAR": 0,
        "SIGMA": 0.05,
        "NEDOS": 2000,
        "LDAU" : ".TRUE.",
        "LDAUU": f"{U} 3 0",
        "LDAUTYPE": 2,
        "LDAUPRINT": 2,
     }
     # creating Incar object
     incar = Incar(incar_dict)
     # calculations directory
     calc_dir = './U_tuning/'
     # setting up destination directory     
     file_dir = calc_dir + 'U_' + str(U) + '/'
    # checking if path already exists - if not create it
     if not os.path.exists(file_dir):
            os.makedirs(file_dir)
    
    # writing INCAR in designated path
     incar.write_file(file_dir + 'INCAR')
     
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
                  new_job_vasp_file.write(f'#SBATCH --job-name=A_PBE+U{U} \n')
              else:
                  # if line doesn't contain job-name write same line from reference job_vasp.sh
                  new_job_vasp_file.write(line)                             
           new_job_vasp_file.close()       
      
    
#              
