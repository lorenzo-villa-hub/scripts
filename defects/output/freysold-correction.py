#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  2 11:21:53 2019

@author: villa
"""
##############################################################################
# important to use on spyder - opens a separate window for the plot
import matplotlib
matplotlib.use('TkAgg')
#############################################################################

import matplotlib.pyplot as plt
import numpy as np
import sys
import re
from glob import glob
import os

##################################################################################
print('')
print('usage : freysold_correction.py  def_calc_directory  pure_calc_directory')
print('')
print('Enter CUT-OFF ENERGY and DIELECTRIC CONSTANT in the code')
print('')
print('This code uses the original code "sxdefectalign", for info consult the original manual')
print('"sxdefectalign" needs to be added in your .bashrc')  
print('')
####################################################################################

# cut-off energy of VASP calculations in eV 
E_cut = 500 # (eV)
# original code needs it in Ry
E_cut = E_cut / 13.605698066 # ( Ry)

# Dielectric constant
epsilon = 5.26


# input paths
#path of defect calculation 
input_dir_def = sys.argv[1] + '/'
#path of pure calculation
input_dir_pure = sys.argv[2] + '/'

#adding wild card to input directory
wild_dir = (input_dir_def + '*/')
# creating a list of directories contained in input directory
list_dir = glob(wild_dir)

# reading info file for removed atom coordinates
info_file = open(input_dir_def + 'info', 'r')
# reading 1st line
coord = info_file.readline()
# reading 2nd line where coord are written 
coord = info_file.readline()
# eliminating the spaces in the line
coord = coord.strip()

#saving original work directory
orig_work_dir = os.getcwd()

for dir in list_dir:
    
    # going back to original work dir
    os.chdir(orig_work_dir)
    
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # finding charge state from subfolder name
    q = subfold.replace('Charged','')
    q = int(q)
    
    if q != 0:
        print('')
        print('')
        print('')
        print(f'Charge state = {q}')
        print('')
        print('')
        print('')
    
        path_def = dir + '1-PBE-SCF/LOCPOT'
        path_pure = input_dir_pure + '1-PBE-SCF/LOCPOT'

        # creating charge states directories for corrections
        corr_charge_path = f'./Charged{q}'
        if not os.path.exists(corr_charge_path):
            os.makedirs(corr_charge_path)
        else:
            print(f'Directory {corr_charge_path} already exists') 
        
        # setting work directory in charge state folder    
        os.chdir(corr_charge_path)
        
        # remember to put the minus in front of the charge state because the code works putting in front of charge the n° of e- added/removed to the system
        q_command = -1*q
        # executing sxdefectalign code on linux with chosen input values
        command_line = f'sxdefectalign --ecut {E_cut} --charge {q_command} --eps {epsilon} --center {coord} --relative --vdef {path_def}  --vref {path_pure} --vasp'
        output = os.system(command_line)

    
    if q != 0:

###############################################################################################################
# PLOTTING AND SELECTING C VALUE	
################################################################################################################

        # takes original 'vline-eV-a0.dat' and separates in 2 files where the '&' is present in the original file     
        new_file_1 = open('vline-eV-a0_1.dat','w')
        new_file_2 = open('vline-eV-a0_2.dat','w')
        new_file = False
        with open('./vline-eV-a0.dat') as origin_file:
              # searching lines in input file   
               for line in origin_file:
                 # emulating 'grep' command
                  sel_line = re.findall(r'&', line)
                  # if line is the job line print customized job name                      
                  if sel_line:
                      new_file = True                        
                  if new_file == False:
                      if line != '&\n':
                          new_file_1.write(f'{line}\n')    
                  if new_file == True:
                      if line!= '&\n':
                          new_file_2.write(f'{line}\n')                                    
        new_file_1.close()    
        new_file_2.close()
        
        
        data1 = np.loadtxt('vline-eV-a0_1.dat', 'f')
        data2 = np.loadtxt('vline-eV-a0_2.dat', 'f')                 
        
        plt.plot(data1[:,0],data1[:,1] , label = 'long-range')
        plt.plot(data2[:,0],data2[:,1] , label = 'from DFT')
        plt.plot(data2[:,0],data2[:,2] , label = 'short-range')
        plt.xlabel('z (bohr)')
        plt.ylabel('Potential (V)')
        plt.title(f'Charged {q}')
        plt.legend()
        
        # get maximum and minimum of oscillation to obtain C value from average
        
        print('')
        print("Please click on max and min of the oscillations in short-range potential to get C value from average")
        x = plt.ginput(2, timeout = 0)
        C = (x[0][1] + x[1][1])/2
                
        plt.close()
        
        print('')
        print('Data acquired')
        print('')
        print(f'q = {q} C = {C}')
        print('')
###########################################################################################################################        
        
        # create file with sxdefectalign output
        output_file = open('freysold_corr_output', 'w')
        
        # execute second part of sxdefectalign with C value as additional input 
        command_line = f'sxdefectalign --ecut {E_cut} --charge {q_command} --eps {epsilon} --center {coord} --relative --vdef {path_def}  --vref {path_pure} --vasp -C {C}'
        output = os.popen(command_line).readlines()
        
        # writing output files   
        for l in range(0,len(output)):
            output_file.write(output[l])
        output_file.close()
        
        
