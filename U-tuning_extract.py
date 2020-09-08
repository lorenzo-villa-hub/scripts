#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul 17 09:47:27 2019

@author: villa
"""

import os
import sys
from glob import glob
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp import Poscar

print(' Usage : U-tuning_extract.py   path_to_U-tuning_calc ')

input_dir = sys.argv[1]
#input_dir = '/nfshome/villa/local-data/AN_cubic/DOS-BS/PBE+U/U_tuning'

system_name = os.path.basename(input_dir)

input_dir = (input_dir + '/')

#opening output file
file_name = f'{system_name}-a-Eg.dat'
file_table = open(file_name,'w') 
# system name on file
file_table.write('# %s \n' % (system_name))
# legend on file
file_table.write('#U_par  a  E_gap \n')                 
   
#adding wild card to input directory
wild_dir = (input_dir + '*/')
#creating a list of directories contained in input directory
list_dir = glob(wild_dir)

# search in every directory of the list
for dir in list_dir:
   
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # finding charge state from subfolder name
    U_par = subfold.replace('U_','')    
    U = float(U_par)
    
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
    file_table.write('%f  %f  %f \n' % (U , lattice_par ,  gap))
    
    

file_table.close()
print(f'Data printed in "{system_name}-a-Eg.dat"')
print('')


import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(18,10))

import matplotlib 

matplotlib.rcParams.update({'font.size': 22})


data = np.loadtxt(f"{system_name}-a-Eg.dat",'f')

U = data[:,0]
a = data[:,1]
Eg = data[:,2]


# plot U vs a
plt.subplot(1,2,1)

plt.xlabel('Coulomb parameter (eV)')
plt.ylabel('Lattice parameter ($\AA$)')
plt.grid()
plt.title('NaNbO3')
plt.scatter(U,a, marker = (4,0), s = 500)
plt.hlines(3.96 , 0, max(U), linestyles = 'dashed', label = 'Exp = 3.96')
plt.legend()


# plot U vs Eg
plt.subplot(1,2,2)
plt.xlabel('Coulomb parameter (eV)')
plt.ylabel('Energy gap (eV)')
plt.grid()
plt.scatter(U,Eg, marker = (4,0), s = 500, color = 'r')
plt.hlines(3.29 , 0, max(U) , linestyles = 'dashed', label = 'Exp = 3.29')
plt.title('NaNbO3')
plt.legend()


# save plot
plt.savefig('NN_U_tuning.png')
print('Plot saved in current folder')
