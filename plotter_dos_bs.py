#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter , BSDOSPlotter
import os

dos_path = '../2-PBE-DOS'
bs_path = '.'

vasprun_dos = Vasprun(os.path.join(dos_path,"vasprun.xml"))
if bs_path is not dos_path:
    vasprun_bs = Vasprun(os.path.join(bs_path,"vasprun.xml"))
else:
    vasprun_bs = vasprun_dos


dos = vasprun_dos.complete_dos
efermi = dos.efermi

bs = vasprun_bs.get_band_structure(line_mode=True,force_hybrid_mode=False,efermi=efermi)

plt = BSDOSPlotter(bs_projection=None, dos_projection='elements', bs_legend=None).get_plot(bs,dos)

plt.show()
