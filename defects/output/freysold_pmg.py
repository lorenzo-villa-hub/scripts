#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Nov 22 11:41:35 2019

@author: villa
"""
import numpy as np
from pymatgen.io.vasp.inputs import Poscar
from pymatgen.io.vasp.outputs import Vasprun, Locpot, VolumetricData
from pymatgen.analysis.defects.core import Vacancy , DefectEntry
from pymatgen.analysis.defects.corrections import FreysoldtCorrection
from pymatgen.analysis.defects.generators import SubstitutionGenerator
#from pymatgen.core.structure import Structure

Epure = -799.45169854
structure_pure = Poscar.from_file('POSCAR_supercell').structure
structure_defect = Poscar.from_file('POSCAR').structure
structure_defect_init = Poscar.from_file('POSCAR_defect_init').structure
uncorrected_energy = Vasprun('vasprun.xml').final_energy - Epure

locpot_pure = Locpot.from_file('LOCPOT_pure')
vol_data_pure = VolumetricData(structure_pure,locpot_pure.data)

locpot_defect = Locpot.from_file('LOCPOT_defect')
vol_data_defect = VolumetricData(structure_defect,locpot_defect.data)

# reading info file
with open('info','r') as info_file:
    coord = info_file.readlines()[1].split(',')
    defect_site_coord = np.around(np.asarray(coord,dtype = np.float64), decimals = 4)
    
# finding defect site
for site in structure_pure.sites:
    if np.array_equiv(np.around(site.frac_coords,4),defect_site_coord):
        defect_site = site

parameters = {}
parameters['axis_grid'] = []
parameters['bulk_planar_averages'] = []
parameters['defect_planar_averages'] = []
for i in range(0,3):
    parameters['axis_grid'].append(vol_data_pure.get_axis_grid(i))
    parameters['bulk_planar_averages'].append(vol_data_pure.get_average_along_axis(i))
    parameters['defect_planar_averages'].append(vol_data_defect.get_average_along_axis(i))
parameters['initial_defect_structure'] = structure_defect_init
parameters['defect_frac_sc_coords'] = defect_site_coord    

#        
vacancy = Vacancy(structure_pure, defect_site, charge=2.0, multiplicity=None)

defect_entry = DefectEntry(vacancy,uncorrected_energy,corrections=None,parameters=parameters,entry_id='test')

freysold_class = FreysoldtCorrection(10,energy_cutoff=500)

freysold_corrections = freysold_class.get_correction(defect_entry)

freysold_class.plot(0)



