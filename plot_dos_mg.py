#!/nfshome/villa/anaconda3/bin/python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter, DosPlotter

vaspout = Vasprun("./vasprun.xml")

# Initializes plotter with some optional args. Defaults are usually
# fine,
complete_dos = vaspout.complete_dos
plt = DosPlotter()
plt.add_dos_dict(complete_dos.get_spd_dos())
tdos = vaspout.tdos
# Adds a DOS with a label.
plt.add_dos("Total", tdos)
#plt.get_plot()

plt.save_plot('dos_mg.pdf', img_format='pdf', xlim= [-7,6], ylim=[0,20])
