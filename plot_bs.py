#!/nfshome/villa/anaconda3/bin/python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter

vaspout = Vasprun("vasprun.xml")
bandstr = vaspout.get_band_structure(line_mode=True)
#bandstr = vaspout.get_band_structure(kpoints_filename = 'KPOINTS_labels' , line_mode=True)
plt = BSPlotter(bandstr).get_plot(ylim=[-10,10])
plt.savefig("bandstructure.pdf")

