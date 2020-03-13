#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul  9 14:02:35 2019

@author: villa
"""


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
xmax = 1.75
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
            # value of Emin(x)
            E_plot[j] = E_line_dict[i] 
    # after cycle on charges go on next line        
    lines_file.write(' \n') 
    
lines_file.close()

# writing legend on plot_file
plot_file.write(f'# {system_name} \n')
plot_file.write('# E_fermi  E_form \n')

# writing x and E(x) on plot_file
for j in range (0,npoints):
    plot_file.write('%f %f \n'  %(x_value[j] , E_plot[j]))


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
