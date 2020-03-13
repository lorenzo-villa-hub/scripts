#!/nfshome/villa/anaconda3/bin/python
"""
Spyder Editor

This is a temporary script file.
"""

import os
from pymatgen.core.periodic_table import Element
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.electronic_structure.plotter import DosPlotter

complete_dos = Vasprun('vasprun.xml').complete_dos

partial_dos = complete_dos.get_spd_dos()

dos_plotter = DosPlotter()
dos_plotter.add_dos('Total_dos',complete_dos)
for orbital in partial_dos:
    dos_plotter.add_dos(orbital,partial_dos[orbital])
plt = dos_plotter.get_plot(xlim=(-10,10))
plt.show()
