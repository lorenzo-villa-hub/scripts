#!/usr/bin/env python
from pymatgen.io.vasp.outputs import Vasprun
import json

v = Vasprun('vasprun.xml')
dos = v.complete_dos

with open('complete_DOS.json','w') as f:
    json.dump(dos.as_dict(),f)
