#!/shared/apps/.intel/2019/python/3.6.8/bin/python

import os
from glob import glob
import sys

"""
Script to submit multiple jobs
insert common path as argument - Usage: submit_jobs_argv.py  path_with_wildcards job_script_name
If job_script_name is not provided default name is 'job_vasp.sh'
"""

path = os.path.abspath(sys.argv[1])
job_script_name = sys.argv[2] if len(sys.argv)>2  else 'job_vasp.sh'
#path = # creating a list of directories contained in input directory
list_dir = glob(path) 
for dir in list_dir:
    
    print(dir)
    print('') 
    os.chdir(dir) 
    os.system('sbatch %s' %job_script_name)
    print('') 

