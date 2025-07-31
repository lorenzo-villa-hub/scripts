#!/usr/bin/env python

from pymatgen.io.lammps.data import LammpsData
import sys

print('Usage: make_lammpsdata_supercell <lammpsdata_filename>  <supercell_size>  <atom_style> (default:atomic)')

lammpsdata_file = sys.argv[1]
supercell_size = sys.argv[2]
atom_style = sys.argv[3] if len(sys.argv) > 4 else 'atomic'

structure = LammpsData.from_file(lammpsdata_file,atom_style=atom_style).structure
structure.make_supercell(supercell_size)
lammpsdata_file = lammpsdata_file.replace('.data','')
LammpsData.from_structure(structure=structure,atom_style=atom_style).write_file(f'{lammpsdata_file}_supercell_{supercell_size}.data')