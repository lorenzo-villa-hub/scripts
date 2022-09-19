#!/usr/bin/env python
from pymatgen.io.vasp.outputs import BSVasprun
import json
import sys


hybrid = bool(sys.argv[1]) if len(sys.argv) > 1 else False
pr_eigen = bool(sys.argv[2]) if len(sys.argv) > 2 else False

v = BSVasprun('vasprun.xml',parse_projected_eigen=pr_eigen)
bs = v.get_band_structure(force_hybrid_mode=hybrid)

with open('BS.json','w') as f:
    json.dump(bs.as_dict(),f)
