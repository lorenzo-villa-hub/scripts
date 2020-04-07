
import json
from pymatgen import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter
from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from my_functions.phase_diagram.analysis import PDHandler, ChempotAnalysis


#if __name__ == "__main__":
MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)
system = ['Ca', 'C' , 'O']  # system we want to get PD for
system_name = '-'.join(system)

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections
pd = PhaseDiagram(processed_entries)

stable_phases = {}
computed_phases = {}

print ('Stable Entries (formula, materials_id)\n--------')
for e in pd.stable_entries:
    print (e.composition.reduced_formula, e.entry_id)
    stable_phases.update({e.composition.reduced_formula : e.entry_id})
    comp = e.composition.get_reduced_composition_and_factor()[0]
    factor = e.composition.get_reduced_composition_and_factor()[1]
    energy = e.energy/factor
    computed_phases[comp.reduced_formula] = energy

with open(f'pmg_stable_phases_{system_name}.json','w') as file:
    json.dump(computed_phases,file)

with open(f'pmg_stable_phases_id_{system_name}.json','w') as file:
    json.dump(stable_phases,file)