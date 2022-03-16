#!/usr/bin/env python

import os
from pynter.automations.vasp import Schemes

dirs = Schemes().find_NEB_dirs()

last_dir =  os.path.basename(dirs[-1])

os.system('ovito ./00/POSCAR ./*/CONTCAR ./%s/POSCAR' %last_dir)
