#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 18:23:10 2019

@author: villa
"""

from pymatgen.io.vasp.inputs import Poscar
from pymatgen.analysis.defects.core import Vacancy
from pymatgen.analysis.defects.generators import SubstitutionGenerator
#from pymatgen.core.structure import Structure

unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure
site = struct.sites[0]
vac = Vacancy(struct,site)
struct_super = vac.generate_defect_structure(supercell=(2, 2, 2))
name = vac.name
comp = vac.defect_composition.as_dict()
s = 2    
# CREATING SUPERCELL 
struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
poscar = Poscar(struct)
poscar.write_file('POSCAR_supercell')
poscar = Poscar(struct_super)
poscar.write_file('POSCAR_vac')



