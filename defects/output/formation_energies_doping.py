
import json
import matplotlib.pyplot as plt
from pymatgen.core.composition import Composition
from pymatgen.core.periodic_table import Element
from pymatgen.electronic_structure.dos import CompleteDos
from my_functions.defects.analysis import DefectsAnalysis
from my_functions.phase_diagram.analysis import ChempotAnalysis, Chempots

    
with open('/home/lorenzo/data/NN_cubic/E-form-doping-vacancies-PBE/doping_vacancies_NN_cubic_PBE.json') as f:
    defects_analysis = DefectsAnalysis.from_dict(json.load(f))
    
with open('/home/lorenzo/data/NN_cubic/phase-diagram-PBE/computed_phases_NN_cubic_PBE.json') as f:
    computed_phases = json.load(f)
    
with open('/home/lorenzo/data/NN_cubic/E-form-doping-vacancies-PBE/dos_NN_cubic_PBE.json') as f:
    bulk_dos = CompleteDos.from_dict(json.load(f))    

with open('/home/lorenzo/data/NN_cubic/phase-diagram-PBE/chempots/chempots_boundary_NN_cubic_PBE.json') as f:
    chempots_boundary_delta = json.load(f)

with open('pmg_stable_phases_Ca-C-O.json') as file:
    phases_Ca = json.load(file)
    
with open('pmg_stable_phases_Sr-C-O.json') as file:
    phases_Sr = json.load(file)

chempots_boundary_delta = {res: Chempots.from_dict(chempots_boundary_delta[res]).chempots for res in chempots_boundary_delta}
chempots_analysis = ChempotAnalysis(computed_phases)
chempots_boundary = {res:chempots_analysis.get_chempots_abs(chempots_boundary_delta[res]) for res in chempots_boundary_delta}

# add chempot of Ca taken from pmg phase diagram at boundary btwn CaCO3 and CaO with same ^muO of NaNbO3 reservoirs
# same for Sr with SrCO3 and SrO
for res in chempots_boundary:
    mu_abs = chempots_boundary[res]
    mu_O_delta = chempots_boundary_delta[res][Element('O')]
    
    ca = ChempotAnalysis(phases_Ca)
    chempots_delta = ca.get_chempots_boundary(Composition('CaCO3'), Composition('CaO'),{Element('O'):mu_O_delta})
    chempots_abs = ca.get_chempots_abs(chempots_delta)
    mu_Ca_abs = chempots_abs[Element('Ca')]
    chempots_boundary[res].update({Element('Ca'):mu_Ca_abs})
    
    ca = ChempotAnalysis(phases_Sr)
    chempots_delta = ca.get_chempots_boundary(Composition('SrCO3'), Composition('SrO'),{Element('O'):mu_O_delta})
    chempots_abs = ca.get_chempots_abs(chempots_delta)
    mu_Sr_abs = chempots_abs[Element('Sr')]
    chempots_boundary[res].update({Element('Sr'):mu_Sr_abs})


new_entries = []
# get data excluding one btw Sr or Ca
for d in defects_analysis.defect_entries:
    if 'Sr' not in d.name:
        new_entries.append(d)
        
vbm,band_gap = defects_analysis.vbm, defects_analysis.band_gap
defects_analysis = DefectsAnalysis(new_entries, vbm, band_gap)

index = 0

for res in chempots_boundary:
    r = chempots_boundary[res]
    index += 1
    mue = defects_analysis.equilibrium_fermi_level(r, bulk_dos,temperature=950)
    defects_analysis.plot(r,fermi_level=mue,plotsize=(2.5,2),show_legend=False,title=res,
                          format_legend=True,get_subplot=True,subplot_settings=[2,3,index])


plt.legend()
#plt.savefig('Sr-doping_vacancies_NN_cubic_PBE.pdf')




