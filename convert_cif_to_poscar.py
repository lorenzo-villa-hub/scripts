#!/usr/bin/env python

from pymatgen.io.cif import CifParser
from pymatgen.io.vasp.inputs import Poscar
import sys

print('Usage: python convert_cif_to_poscar.py <filename> <primitive (bool)> ')

filename = sys.argv[1]
if len(sys.argv) == 3:
	primitive = sys.argv[2]
else:
	primitive = False
	
structure = CifParser(filename).get_structures(primitive=primitive)[0]

Poscar(structure).write_file('POSCAR_'+ filename.replace('.cif',''))
