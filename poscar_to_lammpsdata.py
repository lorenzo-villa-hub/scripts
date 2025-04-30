#!/usr/bin/env python

from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.lammps.data import LammpsData
import sys

print('Usage: poscar_to_lammpsdata <poscar_filename> <lammpsdata_filename> <atom_style> (default:atomic)')

poscar_file = sys.argv[1]
lammpsdata_file = sys.argv[2]
atom_style = sys.argv[3] if len(sys.argv) > 4 else 'atomic'

structure = Poscar.from_file(poscar_file).structure
lammpsdata = LammpsData.from_structure(structure=structure,atom_style=atom_style)
lammpsdata.write_file(lammpsdata_file)