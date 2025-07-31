#!/usr/bin/env python

import ase.io
import os

os.rename('structure.data','previous_structure.data')
atoms = ase.io.read('structure.dump',index=-1)
ase.io.write(filename='structure.data',images=atoms,format='lammps-data')

