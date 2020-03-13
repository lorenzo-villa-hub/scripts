#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 09:47:27 2019

@author: villa
"""

print( 'Script for extracting lattice parameter and Eg value from multiple hybrid calculations')
print('Usage HF-tuning_extract.py  path_to_HF-tuning_calc')


import os
import sys
from glob import glob
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp import Poscar

input_dir = sys.argv[1]
#input_dir = '/nfshome/villa/local-data/NN_cubic/DOS-BS/HSE06/HF_tuning'

system_name = os.path.basename(input_dir)

input_dir = (input_dir + '/')

#opening output file
file_name = f'{system_name}-a-Eg.dat'
file_table = open(file_name,'w') 
# system name on file
file_table.write('# %s \n' % (system_name))
# legend on file
file_table.write('#HF_par  a  E_gap \n')                 
   
#adding wild card to input directory
wild_dir = (input_dir  + '*/')
#creating a list of directories contained in input directory
list_dir = glob(wild_dir)

# search in every directory of the list
for dir in list_dir:
   
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # finding charge state from subfolder name
    HF_par = subfold.replace('HF_','')
    HF = float(HF_par)
    # build path of OUTCAR file
    path_vasprun = (dir + 'vasprun.xml')
    
    # reading vasprun.xml and CONTCAR with pymatgen
    contcar = Poscar.from_file(dir + "CONTCAR")
    structure = contcar.structure.as_dict()
    # getting lattice parameter
    lattice_par = structure['lattice']['a']
    
    # reading VASP OUTPUT
    vasprun = Vasprun(path_vasprun)
    #getting energy gap
    (gap, cbm, vbm, is_direct) = vasprun.eigenvalue_band_properties
        
    # writing output file with charge state and total enery
    file_table.write('%f  %f  %f \n' % (HF , lattice_par ,  gap))
    
    

file_table.close()
print(f'Data printed in "{system_name}-a-Eg.dat"')
print('')


import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(18,10))

import matplotlib 

matplotlib.rcParams.update({'font.size': 22})


data = np.loadtxt(f"{system_name}-a-Eg.dat",'f')

HF = data[:,0]
a = data[:,1]
Eg = data[:,2]


# plot HF vs a
plt.subplot(1,2,1)

plt.xlabel('HF exchange (eV)')
plt.ylabel('Lattice parameter ($\AA$)')
plt.grid()
plt.title('NaNbO3')
plt.scatter(HF,a, marker = (4,0), s = 500)

# plot HF vs Eg
plt.subplot(1,2,2)
plt.xlabel('HF exchange (eV)')
plt.ylabel('Energy gap (eV)')
plt.grid()
plt.scatter(HF,Eg, marker = (4,0), s = 500, color = 'r')
plt.title('NaNbO3')

# save plot
plt.savefig('AN_HF_tuning.png')
print('Plot saved in current folder')
