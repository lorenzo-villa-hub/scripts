# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from pymatgen.ext.matproj import MPRester
from pymatgen.io.vasp.inputs import Incar, Poscar , Kpoints , Potcar, VaspInput

###############################################################################

mp_id = 'mp-3671' # material's ID of compound to calculate

potcar_choices = {'Na':'Na',
                  'Nb':'Nb_pv',
                  'O':'O'}

###############################################################################


API_KEY = "DSR45TfHVuyuB1WvP1"
with MPRester(API_KEY) as mpr:
        
    struct = mpr.get_structure_by_material_id(mp_id, final = False,
                                              conventional_unit_cell = True)
    
    #calculation directory name
    file_dir = struct.composition.reduced_formula
    
    
    # INCAR
    incar_dict = {
    #"KPAR": 4,
    #"NPAR": 24,
    # "NBANDS": 96,
    "ISTART": 0,
    "ICHARG": 2,
    "IBRION": 2,
    "NSW": 100,
    "ISIF": 3,
    "EDIFFG": -0.05,
    "ISPIN": 1,
    "LWAVE": ".TRUE.",
    "LCHARG": ".TRUE.",
    "LORBIT":10,
    "ENCUT": 500,
    "EDIFF": 1e-06,
    "ISMEAR": 0,
    "SIGMA": 0.05,
    "ALGO": "All",
    "ISYM": 2,
    "AMIX": 0.2,
    "LREAL": ".FALSE.",
    # "LVTOT": ".TRUE."        
    # "NEDOS": 2000,
    #    "LHFCALC" : ".TRUE.",
    #    "HFSCREEN": 0.2,
    #    "NKRED": 2,
    #    "PRECFOCK": "Fast",
    #    "AEXX": 0.24,
    } 
    # creating Incar object from 'incar_dict'
    incar = Incar(incar_dict)
    
    # POSCAR
    poscar = Poscar(struct)
    poscar_dict = poscar.as_dict()
    
    # KPOINTS    
    k = 7
    kpoints = Kpoints.gamma_automatic(
    kpts=(k, k, k), shift=(0.0, 0.0, 0.0)
     )
    
    # get POTCAR with right order from POSCAR
    # check for prevoius element - if it's the same don't write it twice
    prevoius_el = None
    # initializing potcar_symbols
    potcar_symbols = []
    #getting sites list
    sites = poscar_dict['structure']['sites']
    # getting label for element on site
    for site_index in range(0,len(sites)) :
        el = sites[site_index]['label']
        # write only if it is different from the prevoious one
        if prevoius_el != el:                    
            potcar_symbols.append(potcar_choices[el])
            prevoius_el = el
    # set Potcar object        
    potcar = Potcar(symbols=potcar_symbols, functional='PBE', sym_potcar_map=None)

    VASP_input = VaspInput(incar,kpoints,poscar,potcar)
    VASP_input.write_input(file_dir, make_dir_if_not_present=True)
    
    
    
