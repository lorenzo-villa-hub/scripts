#!/shared/apps/.intel/2019/python/3.6.8/bin/python

import os
import sys
from pymatgen.ext.matproj import MPRester
from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints , Potcar, VaspInput

print(' Usage : get_POSCAR_MP.py  materials-ID [conventional]\n')
print(' Add "conventional" to arguments to get conventional unit cell structure in POSCAR\n')

mp_id = str(sys.argv[1])
get_conventional = False
if len(sys.argv) > 2: 
    if str(sys.argv[2]) == 'conventional':
        get_conventional = True
        
wdir = os.getcwd()

API_KEY = "DSR45TfHVuyuB1WvP1"
with MPRester(API_KEY) as mpr:

    structure = mpr.get_structure_by_material_id(mp_id, final = False,
                                              conventional_unit_cell = get_conventional)
    phase = structure.composition.reduced_formula
    Poscar(structure).write_file(wdir + '/POSCAR_' + phase)
