#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 18:23:10 2019

@author: villa
"""
import os
import unittest
import numpy as np

from pymatgen.core import Lattice
from pymatgen.util.testing import PymatgenTest
from pymatgen.io.vasp import Vasprun, Poscar, Outcar
from pymatgen.analysis.defects.core import DefectEntry, Vacancy
from pymatgen.analysis.defects.corrections import FreysoldtCorrection,\
            BandFillingCorrection, BandEdgeShiftingCorrection, KumagaiCorrection
from pymatgen.analysis.defects.utils import generate_R_and_G_vecs



#from pymatgen.core.structure import Structure

unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure

s = 3   
struct.make_supercell(3)
struct = struct
vac = Vacancy(struct, struct.sites[130], charge=0)
vac_dict = vac.as_dict()
ids = vac.generate_defect_structure(1)
poscar = Poscar(ids)
poscar.write_file('POSCAR_def')

#
# 
## CREATING SUPERCELL 
#struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
#struct_dict = struct.as_dict()
#poscar = Poscar(struct)
#poscar.write_file("POSCAR_supercell")
#
#index = 1
#coord = [0,0,1]
#struct.insert(index,'Na',coord)
## create POSCAR
#poscar_int = Poscar(struct)
#poscar_int.write_file('POSCAR_interstitial')
#
#
