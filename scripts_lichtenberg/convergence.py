#!/shared/apps/.intel/2019/python/3.6.8/bin/python
from pymatgen.io.vasp.outputs import Vasprun
from my_functions.job_status import job_status

conv_el = False
conv_ionic = False

try:

	vasprun = Vasprun('vasprun.xml')
	conv_el = vasprun.converged_electronic
	conv_ionic = vasprun.converged_ionic

except:
	status = job_status()
	if status != 'RUNNING' and status != 'PENDING':
		print('Calculation probably FAILED\n')

print(f'Electronic convergence: {conv_el}')
print(f'Ionic convergence: {conv_ionic}')

