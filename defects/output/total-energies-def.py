# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 00:52:38 2019

@author: Lorenzo
"""

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
  file_name = f'E_tot-{system_name}.dat'
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
    ch_state = float(ch_state)
    # build path of OUTCAR file
    path_to_outcar = (dir + '2-PBE-OPT/OUTCAR')

    # reading OUTCAR file for total energy
    E = read_outcar(path_to_outcar)
    # writing output file with charge state and total enery
    file_table.write('%i  %f \n' % (ch_state , E))
    print(ch_state,E)
    
    
  file_table.close()          
            
            
total_energies('/nfshome/villa/local-data/AN_cubic/supercell/O-vacancy')            
#  "/nfshome/villa/Nb-vacancy/*/"        
        






























    