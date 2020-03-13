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

# atom types
A = 'Na'
B = 'Nb'
# setting index and getting coordinates of atoms to exchange
index_a = 1
coord_a = struct_dict['sites'][index_a]['abc']
index_b = 30 
coord_b = struct_dict['sites'][index_b]['abc']

# remove atom A in index A and insert atom B in index A and viceversa
struct.remove_sites([index_a])
struct.insert(index_a, B ,coord_a)

struct.remove_sites([index_b])
struct.insert(index_b, A ,coord_b)

# create POSCAR
poscar_int = Poscar(struct)
poscar_int.write_file('POSCAR_anti-site')


