#!/usr/bin/env python
from pymatgen.io.vasp.outputs import Vasprun

conv_el = False
conv_ionic = False

try:

	vasprun = Vasprun('vasprun.xml')
	conv_el = vasprun.converged_electronic
	conv_ionic = vasprun.converged_ionic

except:
	print('Calculation probably failed or not finished\n')

print(f'Electronic convergence: {conv_el}')
print(f'Ionic convergence: {conv_ionic}')

