#!/nfshome/villa/anaconda3/bin/python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter

vaspout = Vasprun("./vasprun.xml")

(gap, cbm, vbm, is_direct) = vaspout.eigenvalue_band_properties

print(f'Energy gap is {gap} eV')

if is_direct:
  print('Direct gap')
else:
  print('Indirect gap')

print(f'CBM is at {cbm} eV')
print(f'VBM is at {vbm} eV')

