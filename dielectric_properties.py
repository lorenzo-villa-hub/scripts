#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from pymatgen.io.vasp.outputs import Vasprun

vaspout = Vasprun("./vasprun.xml")
vaspout_dict = vaspout.as_dict()
incar_input = vaspout.incar
print('')
if 'LOPTICS' in incar_input:
    if incar_input['LOPTICS'] == True:
        print('"LOPTICS = .TRUE." found in INCAR \n')
        epsilon = vaspout.dielectric
        real_part_0 = epsilon[1][0]
        print('Real part of dielectric function at 0 frequency:')
        print(real_part_0)
        print('')
if 'LEPSILON' in incar_input:
    if incar_input['LEPSILON'] == True:        
        print('"LEPSILON = .TRUE." found in INCAR \n')
        if incar_input['IBRION'] == 8:
            print('"IBRION = 8" found in INCAR \n')
        epsilon_static = vaspout.epsilon_static
        print('Macroscopic static dielectric tensor:')
        print(epsilon_static)
        epsilon_ionic = vaspout.epsilon_ionic
        print('ionic part of the static dielectric constant. Present when it’s a DFPT run:')
        print(epsilon_ionic)
        print('')



