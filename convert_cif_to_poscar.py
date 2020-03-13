
from pymatgen.io.cif import CifParser
from pymatgen.io.vasp.inputs import Poscar

structure = CifParser('NN_R3c_Seidel1976.cif').get_structures(primitive=False)[0]

Poscar(structure).write_file('POSCAR_NN_R3c_conventional')