#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from monty.json import MontyEncoder
import json


v = Vasprun('vasprun.xml')
dos = v.complete_dos

d = dos.as_dict()
with open('complete_DOS.json','w') as file:
    	json.dump(d,file,cls=MontyEncoder)
