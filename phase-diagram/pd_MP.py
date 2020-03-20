"""
This is a basic example of how to create, plot, and analyze Phase Diagrams using the pymatgen
codebase and Materials Project database. To run this example, you should:
* have pymatgen (www.pymatgen.org) installed along with matplotlib
* obtain a Materials Project API key (https://www.materialsproject.org/open)
* paste that API key in the MAPI_KEY variable below, e.g. MAPI_KEY = "foobar1234"
For citation, see https://www.materialsproject.org/citing
For the accompanying comic book, see http://www.hackingmaterials.com/pdcomic
"""

from pymatgen import MPRester
from pymatgen.entries.compatibility import MaterialsProjectCompatibility
from pymatgen.analysis.phase_diagram import PhaseDiagram, PDPlotter
from pymatgen.core.periodic_table import Element
from pymatgen.core.composition import Composition
import copy


#if __name__ == "__main__":
MAPI_KEY = 'DSR45TfHVuyuB1WvP1'  # You must change this to your Materials API key! (or set MAPI_KEY env variable)
system = ["Na", "Nb" , "O"]  # system we want to get PD for

system_name = '-'.join(system)
#opening output file
file_name = f'PD_{system_name}.dat'
file_table = open(file_name,'w') 
# system name on file
file_table.write('# System : %s \n' % (system_name))
# legend on file
file_table.write('# Phase   total_energy p.f.u.(eV) \n') 


mpr = MPRester(MAPI_KEY)  # object for connecting to MP Rest interface
compat = MaterialsProjectCompatibility()  # sets energy corrections and +U/pseudopotential choice

# Create phase diagram!
unprocessed_entries = mpr.get_entries_in_chemsys(system)
processed_entries = compat.process_entries(unprocessed_entries)  # filter and add energy corrections
pd = PhaseDiagram(processed_entries)
pd_dict = pd.as_dict()
# Plot!
plt = PDPlotter(pd, show_unstable=False).get_plot(fontsize = 20, plotsize = 3)  
plt.savefig(f'PD_{system_name}.png')
# you can also try show_unstable=True
#chem_pot_plot = plt.get_chempot_range_map_plot([Element("Ba"), Element("Ti")], fontsize = 14 , plotsize = 1.5)
#chem_pot_plot.savefig('chem_pot_plot_Ba-Ti-O.png')

stable_phases = {}

print ('Stable Entries (formula, materials_id)\n--------')
for e in pd.stable_entries:
    print (e.composition.reduced_formula, e.entry_id)
    stable_phases.update({e.composition.reduced_formula : e.entry_id})
    comp = e.composition.get_reduced_formula_and_factor()
    formula = comp[0]
    energy = e.energy/comp[1] 
    file_table.write(f'{formula}  {energy} \n')


file_table.close()

