#!/nfshome/villa/anaconda3/bin/python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter , BSDOSPlotter

vaspout_bs = Vasprun("vasprun.xml")
vaspout_dos = Vasprun("vasprun.xml")

bandstr = vaspout_bs.get_band_structure(line_mode=True, force_hybrid_mode=True )
bs_data = BSPlotter(bandstr).bs_plot_data()

dos = vaspout_dos.complete_dos
dos_dict = dos.as_dict()

#plt = BSPlotter(bandstr).get_plot()
plt = BSDOSPlotter(bs_projection = None,  dos_projection = 'elements', bs_legend = None, fig_size = (14,11)).get_plot(bandstr,dos)
plt.savefig("dos_bs.png")

