#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.dos import CompleteDos

dos = Vasprun('vasprun.xml').complete_dos
test = CompleteDos.from_dict(dos.as_dict())
print(test)
