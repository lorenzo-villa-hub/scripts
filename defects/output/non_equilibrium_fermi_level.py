
import json
from pymatgen.electronic_structure.dos import CompleteDos
from my_functions.defects.analysis import DefectsAnalysis
from my_functions.phase_diagram.analysis import ChempotAnalysis, Chempots

    
with open('/home/lorenzo/data/NN_cubic/E-form-vacancies-PBE/vacancies_NN_cubic_PBE.json') as f:
    defects_analysis = DefectsAnalysis.from_dict(json.load(f))
    
with open('/home/lorenzo/data/NN_cubic/phase-diagram-PBE/computed_phases_NN_cubic.json') as f:
    computed_phases = json.load(f)
    
with open('/home/lorenzo/data/NN_cubic/E-form-vacancies-PBE/dos_NN_cubic_PBE.json') as f:
    bulk_dos = CompleteDos.from_dict(json.load(f))    

with open('/home/lorenzo/data/NN_cubic/phase-diagram-PBE/chempots/chempots_boundary_NN_cubic_PBE.json') as f:
    chempots_boundary_delta = json.load(f)

chempots_boundary_delta = {res: Chempots.from_dict(chempots_boundary_delta[res]).chempots for res in chempots_boundary_delta}
chempots_analysis = ChempotAnalysis(computed_phases)
chempots_boundary = {res:chempots_analysis.get_chempots_abs(chempots_boundary_delta[res]) for res in chempots_boundary_delta}

mu = chempots_boundary['A']
temperature = 950

# band gap correction -> using band gap of HSE
defects_analysis.band_gap = 2.927

mue = defects_analysis.equilibrium_fermi_level(mu, bulk_dos,temperature=temperature)
defect_concentrations_cubic = defects_analysis.defect_concentrations(mu,temperature=temperature,fermi_level=mue)



with open('vacancies_NN_R3-_PBE.json') as f:
    defects_analysis = DefectsAnalysis.from_dict(json.load(f))
    
with open('dos_NN_R3-_PBE.json') as f:
    bulk_dos = CompleteDos.from_dict(json.load(f))
    
with open('/home/lorenzo/data/NN_R3-/phase-diagram-PBE/computed_phases_NN_R3.json') as f:
    computed_phases = json.load(f)
    
with open('/home/lorenzo/data/NN_R3-/phase-diagram-PBE/chempots/chempots_boundary.json') as f:
    chempots_boundary_delta = json.load(f)
      

chempots_boundary_delta = {res: Chempots.from_dict(chempots_boundary_delta[res]).chempots for res in chempots_boundary_delta}
chempots_analysis = ChempotAnalysis(computed_phases)
chempots_boundary = {res:chempots_analysis.get_chempots_abs(chempots_boundary_delta[res]) for res in chempots_boundary_delta}

mu = chempots_boundary['A']

defects_analysis.band_gap = 5.5264 #band gap correction with HSE value

frozen_defect_concentrations = defect_concentrations_cubic
frozen_fermi_level = defects_analysis.non_equilibrium_fermi_level(frozen_defect_concentrations,mu,bulk_dos,temperature=200)

fermi_level = defects_analysis.equilibrium_fermi_level(mu, bulk_dos, temperature=200)




