
import json
from pymatgen import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter,PDEntry
from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from pymatgen.io.vasp.outputs import Vasprun



MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)
system = ['Ag', 'Nb' , 'O']  # system we want to get PD for
system_name = '-'.join(system)

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections

pd = PhaseDiagram(processed_entries)
pd_dict = pd.as_dict()

with open(f'PD_{system_name}.json','w') as f:
    json.dump(pd_dict,f)
