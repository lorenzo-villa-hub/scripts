import sys
import numpy as np
import os
from glob import glob
import re


print('')
print('Usage : get-E-form-freysold.py   path_to_def_form_energies   path_to_freysold_corr_folder')

#input_E_form = '/nfshome/villa/local-data/NN_cubic/E-form-defects-code/O-vacancy/'
#input_frey = '/nfshome/villa/local-data/NN_cubic/freysold-corrections/O-vacancy/'
input_E_form = sys.argv[1] + '/'
input_frey = sys.argv[2] + '/'

# get name of system from folder name
path = os.path.dirname(input_E_form)
system_name = os.path.basename(path)

data_E_form = np.loadtxt(input_E_form + system_name + '-E-form.dat' , 'f')
# dictonary with form energies
E_form_dict = {}
for i in range(0,len(data_E_form)):
    E_form_dict[data_E_form[i,0]] = data_E_form[i,1]
# dictonary with corr energies
corr_dict = {}
#dictonary with corrected energies
final_dict = {}
#adding wild card to input directory
wild_dir = (input_frey + '*/')
# creating a list of directories contained in input directory
list_dir = glob(wild_dir)

for dir in list_dir:
    
    path=os.path.dirname(dir) 
    # identifying last folder of path
    subfold = os.path.basename(path)
    # finding charge state from subfolder name
    q = subfold.replace('Charged','')
    q = int(q)
      
    with open (dir + 'freysold_corr_output','r') as origin_file:
    # searching lines in input file   
        for line in origin_file:
       # emulating 'grep' command
            corr_line = re.findall(r'Defect correction', line)
            if corr_line:
               # splits line  
               line = line.split()
               # the E value is the 5th string in line - convert to float
               corr_energy = float(line[3])
               
    
    corr_dict[q] = corr_energy
               
    
for charge in E_form_dict:
    if charge == 0:
        final_dict[charge] =  E_form_dict[charge]                                              
    else:
        final_dict[charge] = E_form_dict[charge] + corr_dict[charge] 

out_file = open('corrected_E-form.dat','w')
out_file.write('#q   E-form-corr\n')

for charge in final_dict:
    out_file.write('%i  %f \n' %(charge , final_dict[charge]))

out_file.close()               
        
