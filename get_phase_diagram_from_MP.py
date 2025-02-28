#!/usr/bin/env python

import json
import sys
from pymatgen.ext.matproj import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram
from pynter.tools.utils import save_object_as_json


print("Usage: get_phase_diagram_from_MP.py 'Element1,Element2,Element3,...'")

system = [el for el in sys.argv[1].split(',')]  # system we want to get PD for

MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)
system_name = '-'.join(system)

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

unprocessed_entries = mpr.get_entries_in_chemsys(system,inc_structure=True)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections

pd = PhaseDiagram(processed_entries)

filename = f'PD_{system_name}.json'

save_object_as_json(pd,filename)

print(f"PhaseDiagram object saved as dict in {filename}")
