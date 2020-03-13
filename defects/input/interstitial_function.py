#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul 22 18:23:10 2019

@author: villa
"""
from pymatgen.io.vasp.inputs import Poscar

unit_poscar = Poscar.from_file("POSCAR_unit")
struct = unit_poscar.structure

type_elements = ['Na','Nb','O']

###############################################################################
# function to create interstitials
# might take long
def create_interstitial_supercells(structure,element,size=2):
    
    '''return a dictionary with sites (Site object) and supercell
       structures (Structure object) for interstitials of a given pure 
       structure (Structure object) '''
    
    from pymatgen.analysis.defects.utils import StructureMotifInterstitial
    
    # get interstitials with InFiT (Interstitialcy Finding Tool) algorithm - check pymatgen for details
    int_object = StructureMotifInterstitial(structure,element)
    int_sites = int_object.enumerate_defectsites()
    int_supercells = int_object.make_supercells_with_defects([[size,0,0],[0,size,0],[0,0,size]])
    
    interstitials = {}
    interstitial_sites =  {}
    interstitial_structures = {}
    # getting properties and supercells for all interstitial sites
    for i in range(0,len(int_sites)):
        
        int_coord_type = int_object.get_motif_type(i)
        interstitial_sites[int_coord_type] = int_sites[i]
        # first supercell in list is the pure
        struct_int = int_supercells[i+1]
        interstitial_structures[int_coord_type] = struct_int
        
        interstitials['sites'] = interstitial_sites
        interstitials['structures'] = interstitial_structures
        
    return interstitials
##############################################################################

##############################################################################
# MAKING DICTIONARY WITH INTERSTITIALS SUPERCELLS
interstitial_dict = {}

for el in type_elements:
    
    interstitial_dict[el] = create_interstitial_supercells(struct,el,size = 3)
##############################################################################