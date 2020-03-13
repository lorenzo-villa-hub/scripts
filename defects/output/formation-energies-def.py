#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 14:34:20 2019

@author: villa
"""

#############################################################################
#############################################################################
#### FUNCTIONS  #############################################################
#############################################################################
#############################################################################

# function for reading the total energy of the OUTCAR file
def read_outcar (out_file):
  import re
  with open(out_file) as origin_file:
   # searching lines in input file   
   for line in origin_file:
       # emulating 'grep' command
        energy_line = re.findall(r'energy without entropy', line)
        if energy_line:
           # splits line  
           line = line.split()
           # the E value is the 5th string in line - convert to float
           final_energy = float(line[4])
           #print(final_energy)      
  #output                
  return final_energy                

# function for getting total energies
def total_energies (input_dir):
    
  import os 
  from glob import glob  
  
 # add slash for complete path     
  input_dir = (input_dir + '/')
  
  # get name of system from folder name
  path = os.path.dirname(input_dir)
  system_name = os.path.basename(path)
  print(system_name)
  
  #opening output file
  file_name = f'{system_name}-E-tot.dat'
  file_table = open(file_name,'w') 
  # system name on file
  file_table.write('# %s \n' % (system_name))
  # legend on file
  file_table.write('#q  E_tot \n')                 
                   
  #adding wild card to input directory
  wild_dir = (input_dir + '*/')
# creating a list of directories contained in input directory
  list_dir = glob(wild_dir)

  # search in every directory of the list
  for dir in list_dir:
   
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # finding charge state from subfolder name
    ch_state = subfold.replace('Charged','')
    ch_state = int(ch_state)
    # build path of OUTCAR file
    path_to_outcar = (dir + '2-PBE-OPT/OUTCAR')

    # reading OUTCAR file for total energy
    E = read_outcar(path_to_outcar)
    # writing output file with charge state and total enery
    file_table.write('%i  %f \n' % (ch_state , E))
    print(ch_state,E)
    
    
  file_table.close()          
  return file_name          
            

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


file_name = total_energies('/nfshome/villa/local-data/AN_cubic/supercell/O-vacancy') 

system_name = file_name.replace('-E-tot.dat','')

#############################################################################
############ INPUT ##########################################################
#############################################################################

# values of pure structure
Epure = -980.59680141
vbm =  3.0416 # eV
# chemical potential
mu =  -9.86114268/ 2 # Etot/n°atoms

############################################################################
############################################################################
# FORMATION ENERGIES
#############################################################################
#############################################################################

# total energies file
file_name = 'O-vacancy-E-tot.dat'

system_name = file_name.replace('-E-tot.dat','')

import numpy as np
data = np.loadtxt(file_name, dtype='f')

#opening file for formation energies
form_file = open(f'{system_name}-E-form.dat','w')
form_file.write(f'# {system_name} \n')
form_file.write('#q  E_form \n')

# size of input file                
size = data.shape
nrows = size[0]
ncolumns = size[1]

# charge states
q = (data[:,0])
# converting charge into integers
q_int = np.rint(q)
q_int = q_int.astype(int)
# energies
E = data[:,1]

# introducing total energies in dictonary format -> E_dict{q:E(q),...}
E_dict = {}
for i in range (0,nrows):
    E_dict[q_int[i]] = E[i]


#introducing formation energies in dictonary format
E_form_dict = {}

# calculating FORMATION ENERGIES for every charge state
for i in q_int:
    # computing formation energy
    E_form = E_dict[i] - Epure + i*vbm + mu
    form_file.write('%i %f \n' % (i , E_form))
    print(i, E_form) 
    # print formation energies on dictonary
    E_form_dict [i] = E_form
    
form_file.close()   






































