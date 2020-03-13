#!/shared/apps/.intel/2019/python/3.6.8/bin/python

import sys
import os
from glob import glob
from pymatgen.io.vasp.outputs import Vasprun
from my_functions.job_status import job_status

'''Usage: check_convergence_argy.py  path (with wildcard) '''

path = sys.argv[1]
#path = # creating a list of directories contained in input directory
list_dir = glob(os.path.normpath(path))
for dir in list_dir:
	print('')
	print(dir)
	os.chdir(dir)
	if os.path.isfile(dir + '/vasprun.xml'):
		conv_el = False
		conv_ionic = False 
		try:
			vasprun = Vasprun(dir + '/vasprun.xml',exception_on_bad_xml=True)
			conv_el = vasprun.converged_electronic  
			conv_ionic = vasprun.converged_ionic
		except:		
			status = job_status()	
			if status != 'RUNNING' and status != 'PENDING':
				print('\nCalculation probably FAILED\n')
		print(f'electronic convergence: {conv_el}')
		print(f'ionic convergence: {conv_ionic}')
	else:
		print('no vasprun.xml file found')
	print('')

