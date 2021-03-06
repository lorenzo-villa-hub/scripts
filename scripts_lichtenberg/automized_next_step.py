#!/shared/apps/.intel/2019/python/3.6.8/bin/python

import os
from glob import glob
from shutil import copyfile
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp.inputs import Kpoints

script_job_name = 'job_vasp.sh'
script_job_array_name = 'job_vasp_array.sh'

f = open('exit_status.txt','w')

conv_el = False
conv_ionic = False

f.write('Job exit... Analysing output...\n')
f.write('\n')
f.write('"vasprun.xml" analysis ...\n')
f.write('\n')
	
try:
	# check convergence 
	vasprun = Vasprun('vasprun.xml')
	conv_el = vasprun.converged_electronic
	conv_ionic = vasprun.converged_ionic
except:
	f.write('Calculation probably failed or not converged\n')

f.write(f'Electronic convergence: {conv_el}\n')
f.write(f'Ionic convergence: {conv_ionic}\n')
f.write('\n')	

found_next_step = False
# if prevoius step is converged start next step	
if conv_el == True and conv_ionic == True:
	
	# getting name of current directory and relative step number
	current_step_dir_name = os.path.basename(os.getcwd())
	current_step_number = int(current_step_dir_name.split('-')[0])

	list_dir = glob('../*')

	for dir in list_dir:
		# finding next step directory looking at first number in directory
		dir_step_number = int(os.path.basename(dir).split('-')[0])
		if dir_step_number == current_step_number + 1 :
			next_step_number = current_step_number + 1
			next_step_dir = dir
			found_next_step = True
			break
	# if a next step directory has been found proceed with next calculation submission
	if found_next_step:
		
		kpoints_current_step =Kpoints.from_file('KPOINTS').as_dict()
		kpoints_next_step = Kpoints.from_file(next_step_dir + '/KPOINTS').as_dict()
	
		# if KPOINTS of current and next step are the same copy WAVECAR and CHGCAR
		if kpoints_current_step == kpoints_next_step:	

			copyfile('WAVECAR',   next_step_dir + '/WAVECAR')
			copyfile('CHGCAR',    next_step_dir + '/CHGCAR')
			f.write(f'KPOINTS of current and next step are the same: CHGCAR and WAVECAR copied in "{next_step_dir}"'+'\n')
	
		# copy CONTCAR of current step in POSCAr of next step
		copyfile('CONTCAR',   next_step_dir + '/POSCAR')
		f.write(f'CONTCAR copied in POSCAR in "{next_step_dir}"'+'\n')
		f.write(f'Next step calculation in directory "{next_step_dir}" submitted\n')
		# setting directory of next step as working dir and start calculation
		os.chdir(next_step_dir)
		# check if job_vasp_array.sh exists and submit it, else submit normal job_vasp.sh file
		if os.path.isfile(script_job_array_name):
			os.system(f'sbatch {script_job_array_name}')
		else:
			os.system(f'sbatch {script_job_name}')
	
	else:
		f.write('No next step found, no other calculation submitted')

else:
	f.write('Next step not submitted... control manually')


f.close()
