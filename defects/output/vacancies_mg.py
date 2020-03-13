
import numpy as np
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.thermodynamics import DefectPhaseDiagram
from pymatgen.io.vasp.inputs import Poscar

Epure = -1032.98698590

poscar = Poscar.from_file('POSCAR_supercell')
structure = poscar.structure
defect_site = structure.sites[134]
site=defect_site.as_dict()

data = np.loadtxt('O-vacancy-E-tot.dat','f')
charges =[]
Etot = {}
for i in range(0,len(data[:,0])):
    q = data[i,0]
    charges.append(q)
    Etot[q] = data[i,1]

entries = []

for q in charges:
    defect = Vacancy(structure, defect_site, charge = q)
    energy_diff = Etot[q] - Epure
    defect_entry = DefectEntry(defect,energy_diff, corrections=None, parameters=None, entry_id= f'q = {q}' )
    entries.append(defect_entry)

vbm = 1.5365
band_gap = 1.525
dpd = DefectPhaseDiagram(entries, vbm, band_gap)
chempot = {'O':-9.879307/2}
dpd.plot(mu_elts=chempot, xlim=None, ylim=None, ax_fontsize=1.3, lg_fontsize=1.0, lg_position=None, fermi_level=None, title=None, saved=False)
#prova = dpd.as_dict()