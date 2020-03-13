#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 18:23:10 2019

@author: villa
"""

from pymatgen.io.vasp.inputs import Poscar
#from pymatgen.core.structure import Structure

unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure

s = 3    
# CREATING SUPERCELL 
struct.make_supercell([[s,0,0],[0,s,0],[0,0,s]])
struct_dict = struct.as_dict()
poscar = Poscar(struct)
poscar.write_file("POSCAR_supercell")

index = 1
coord = [0,0,1]
struct.insert(index,'Na',coord)
# create POSCAR
poscar_int = Poscar(struct)
poscar_int.write_file('POSCAR_interstitial')


