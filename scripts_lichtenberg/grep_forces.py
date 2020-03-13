#!/usr/bin/env python

from pymatgen.io.vasp.inputs import Poscar
import os

number_of_atoms = Poscar.from_file('POSCAR').structure.num_sites
os.system('grep -A %i "TOTAL-FORCE" OUTCAR | tail -%i' %(number_of_atoms+5, number_of_atoms+7))
