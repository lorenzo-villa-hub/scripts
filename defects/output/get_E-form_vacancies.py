#!/nfshome/villa/anaconda3/bin/python
# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 14:34:20 2019

@author: villa
"""


############################################################################
print('')
print('PLEASE GIVE PATH OF DATA AS INPUT')
print('')
print('PLEASE GIVE VALUES OF Etot_pure, vbm AND chem potential IN THE SCRIPT')
print('')
print('this script will analyse data from defect calculations and produce 5 files: ')
print(' -E-tot.dat for total energies')
print(' -E-form.dat for formation energies')
print(' -lines.dat for all formation energies with respect to the fermi level')
print(' -plot.dat for minimum formation energy with respect to the fermi level')
print(' CTL - for a matrix with charge transition levels')

############################################################################

############ INPUT ##########################################################

# delta chemical potentials in different reservoirs
chempot =   {'A':{'Na':-1.27,
                  'Nb':-1.36,
                  'O':-3.15},
             'B':{'Na':-1.60,
                  'Nb':-1.30,
                  'O':-3.06},
             'C':{'Na':-3.13,
                  'Nb':-8.94,
                  'O':  0},
             'D':{'Na':-2.84,
                  'Nb':-9.22,
                  'O':  0},
             'E':{'Na':-2.2,
                  'Nb':-5,
                  'O':-1.52}
}

# reference chemical potentials
chempot_ref = {'Na':-1.295284,
               'Nb':-10.079199,
               'O': -9.879307/2}

# values of pure structure
Epure = -1032.98698590
vbm =  1.5365 # eV

############################################################################


#############################################################################
#### FUNCTIONS  #############################################################
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
  #print(system_name)
  
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
    
    
  file_table.close()          
  return file_name          
            

#############################################################################
#############################################################################
#############################################################################
#############################################################################
#############################################################################


import sys
input_path = sys.argv[1]
#input_path = '/nfshome/villa/local-data/NN_cubic/vacancies-PBE/Nb-vacancy'

file_name = total_energies(input_path) 


#############################################################################
############################################################################
# FORMATION ENERGIES
#############################################################################
#############################################################################

import numpy as np
import os

# total energies file
system_name = file_name.replace('-E-tot.dat','')
vacancy_type = system_name.replace('-vacancy','')

print('')
print(f'Retrieving energies from {input_path}')
print('')
print(system_name)
print('')

data = np.loadtxt(file_name, dtype='f')
original_dir = os.getcwd()

# cycling over different reservoirs
for res in chempot:
    # back to original working dir
    os.chdir(original_dir)
    file_dir = './' + res
    
    if not os.path.exists(file_dir):
        os.makedirs(file_dir)
    # change working dir for new reservoir
    os.chdir(file_dir)
    print(f' Reservoir {res} \n')

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
    
    print('q  E_form')
    # calculating FORMATION ENERGIES for every charge state
    for i in q_int:
        # computing formation energy
        E_form = E_dict[i] - Epure + i*vbm + chempot_ref[vacancy_type] + chempot[res][vacancy_type] 
        form_file.write('%i %f \n' % (i , E_form))
        print(i, E_form) 
        # print formation energies on dictonary
        E_form_dict [i] = E_form
        
    form_file.close()   
    print('')
    
    ##############################################################################
    ##############################################################################
    # SCRIPT FOR PLOTS ##############################################################
    ###############################################################################
    ###############################################################################
    
    
    # file for plots of ALL charge states
    lines_file = open(f'{system_name}-lines.dat','w')
    #  file for plot of minimum energy
    plot_file = open(f'{system_name}-plot.dat','w')
    
    # starting E fermi value
    xmin = -0.5
    # ending E fermi value
    xmax = 4
    # n° of points for plot
    npoints = 200
    # spacing btw one point and the other
    spacing = np.absolute((xmax-xmin)/npoints)
    
    # DEFINING VARIABLES
    # dictonary for energies for plot
    E_line_dict = {}
    # vector for minimum energy
    E_plot = np.zeros(npoints)
    # vector for fermi level values (x-axis)
    x_value = np.zeros(npoints)
    # vector for most stable charge states
    q_plot = np.zeros(npoints)
    
    #legend for lines_file
    lines_file.write(f'# {system_name} \n')
    lines_file.write('# E_fermi ')
    # printing charge states in legend                 
    for i in q_int:
        lines_file.write('%i ' %(i))
    lines_file.write(' \n')    
    
    # printing on lines 
    for j in range (0,npoints):
        # x value for plotting
        x_value[j] = xmin + spacing*(j-1)
        lines_file.write('%f ' %(x_value[j]))
        # initializing emin variable -> huge value that will be for sure bigger
        emin = 100000000000000
        # for every charge state calculating E(x) value and Emin(x)
        for i in q_int:     
            E_line_dict[i] = E_form_dict[i] + i*x_value[j]
            lines_file.write('%f ' %(E_line_dict[i]))
            if E_line_dict[i] < emin:
                emin = E_line_dict[i]
                q_plot[j]  = i
                # value of Emin(x)
                E_plot[j] = E_line_dict[i] 
        # after cycle on charges go on next line        
        lines_file.write(' \n') 
        
    lines_file.close()
    
    # writing legend on plot_file
    plot_file.write(f'# {system_name} \n')
    plot_file.write('# E_fermi  E_form  stable_charge \n')
    
    # writing x and E(x) on plot_file
    for j in range (0,npoints):
        plot_file.write('%f %f    %i \n'  %(x_value[j] , E_plot[j] , q_plot[j]))
    
    
    plot_file.close()
    
    
    ##############################################################################
    # SCRIPT FOR CHARGE TRANSITION LEVELS ########################################
    ##############################################################################
    
    
    ctl_file = open(f'CTL-{system_name}','w')
    #ctl_file.write(f'{q_int}')
    
    # creating list for charge states
    q_list = []
    # giving values to charge list
    for i in range (0,nrows):
        q_list.append(q_int[i])
    
    # initializing matrix for CTL -> 1st row and 1st column are the charge states
    # interceptions are the CTL
    ctl = np.zeros((nrows+1,nrows+1))
    ## from 2nd to last element write charge states in 1st row and 1st column
    ctl[1:(nrows+1) , 0] = q_int
    ctl[ 0 , 1:(nrows+1)] = q_int
    
    # calculating CTL
    for i in q_int:
        for j in q_int:
            if i != j:
                # indexes for ctl matrix
                i_pointer = q_list.index(i)
                j_pointer = q_list.index(j)
                # CTL value
                ctl[i_pointer+1,j_pointer+1] = (E_form_dict[i]-E_form_dict[j])/(j-i)
                
    
    # print charge trans levels file
    for i in range(0,nrows+1):
        for j in range(0,nrows+1):
            ctl_file.write('%f ' %(ctl[i,j]))
        ctl_file.write(' \n')        
    
    ctl_file.close()    
        
    ##############################################################################
    ##############################################################################
    ##############################################################################


































