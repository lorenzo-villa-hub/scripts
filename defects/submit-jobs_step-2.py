#!/usr/bin/python3

import os
from glob import glob
from shutil import copyfile

steps = ['1-PBE-SCF','2-PBE-OPT','3-PBE-DC']
system_path = '/work/scratch/lv51dypu/NN_cubic/test-supercells/'
path = system_path + '*-supercell/O-vacancy/Charged*/'
#path = # creating a list of directories contained in input directory
list_dir = glob(path + steps[1])
for dir in list_dir:
    
    dir_step_1 = dir.replace(steps[1],steps[0])
    dir_step_2 = dir
    copyfile(dir_step_1 + '/WAVECAR', dir_step_2 + '/WAVECAR')
    copyfile(dir_step_1 + '/CHGCAR', dir_step_2 + '/CHGCAR')
    print(dir)
    print('') 
    os.chdir(dir) 
    os.system(f'sbatch job_vasp.sh')
    print('') 

