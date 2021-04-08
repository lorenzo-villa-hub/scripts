
from pymatgen.io.vasp.outputs import Wavecar, Vasprun, Chgcar
from pymatgen.io.vasp.inputs import Poscar

wc = Wavecar()
poscar = Poscar.from_file('CONTCAR')

kpoint = 0
band = 126 # chosen reading PROCAR file for kpoint Gamma. Partial DOS of d-orbitals big "enough"

chgcar = wc.get_parchg(poscar, kpoint, band)
chgcar.write_file('CHGCAR_k0_band126.vasp')
