#!/nfshome/villa/anaconda3/bin/python
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 19 17:44:10 2020

@author: villa
"""

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter

vaspout = Vasprun("vasprun.xml")
bandstr = vaspout.get_band_structure(line_mode=True)
#bandstr = vaspout.get_band_structure(kpoints_filename = 'KPOINTS_labels' , line_mode=True)
plt = BSPlotter(bandstr).get_plot()
plt.show()