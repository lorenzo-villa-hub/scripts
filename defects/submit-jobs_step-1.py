#!/usr/bin/python3

import os
from glob import glob
from shutil import copyfile

steps = ['1-PBE-SCF','2-PBE-OPT','3-PBE-DC']
system_path = '/work/scratch/lv51dypu/NN_cubic/test-supercells/'
path = system_path + '5-supercell-post/O-vacancy/Charged*/'
#path = # creating a list of directories contained in input directory
list_dir = glob(path + steps[0])
for dir in list_dir:
    
    print(dir)
    print('') 
    os.chdir(dir) 
    os.system(f'sbatch job_vasp.sh')
    print('') 

