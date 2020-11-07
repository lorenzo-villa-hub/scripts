# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.

"""
#%%

import json
from pynter.data.datasets import Dataset
from pymatgen.io.vasp.inputs import VaspInput, Incar,Kpoints,Poscar,Potcar
from pymatgen.core.structure import Structure

ds = Dataset.from_directory('/home/lorenzo/tests/project-test/tutorials/Si-BS-dataset')
    
#%%    
    
from matgendb.creator import VaspToDbTaskDrone    

drone = VaspToDbTaskDrone(collection='test')

for j in ds:
    drone.assimilate(j.path)

#%%
    
from matgendb.query_engine import QueryEngine

qe = QueryEngine(collection='test')

# entries = qe.get_entries({'dir_name':'localhost:/home/lorenzo/tests/project-test/tutorials/Si-BS-dataset/3-PBE-BS'})

entries = qe.get_entries({'chemsys':'Si'},optional_data=['calculations'],inc_structure=True)

#%%

e = entries[0]
inputs = e.data['calculations'][0]['input']
incar = Incar(inputs['incar'])
kpoints = Kpoints.from_dict(inputs['kpoints'])
poscar = Poscar(e.structure)
potcar = Potcar(inputs['potcar'])

vaspinput = VaspInput(incar, kpoints, poscar, potcar)

#%%