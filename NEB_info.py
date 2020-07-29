#!/usr/bin/env python

from pymatgen.analysis.transition_state import NEBAnalysis
import sys

neb = NEBAnalysis.from_dir('.')

print('Energies:\n',neb.energies)
print('Forces:\n',neb.forces)

plot = True if len(sys.argv)==2 else False

if plot:
    neb.get_plot().show()
