
import json
from pymatgen import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter,PDEntry
from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
from pymatgen.io.vasp.outputs import Vasprun




#if __name__ == "__main__":
MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)
system = ['Na', 'C' , 'O']  # system we want to get PD for
system_name = '-'.join(system)

mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections

# vasprun = Vasprun('vasprun.xml')
# comp = vasprun.initial_structure.composition
# energy = vasprun.final_energy
# NBTentry = PDEntry(comp, energy)
# processed_entries.append(NBTentry)

pd = PhaseDiagram(processed_entries)

