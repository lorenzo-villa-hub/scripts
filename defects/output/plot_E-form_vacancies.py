
import matplotlib.pyplot as plt
import numpy as np
import os
from glob import glob

import matplotlib 
#matplotlib.rc('xtick', labelsize=20) 
#matplotlib.rc('ytick', labelsize=20)
matplotlib.rcParams.update({'font.size': 22})


defect_types = ['Na','Nb','O']
reservoirs = []
fun_xc = 'PBE+U'
get_single_plots = False

xmin = -0.5
xmax = 3.75
cbm = 3.25
ymin = -6
ymax = 12

# getting names of reservoirs
#adding wild card to directory
wild_dir = (f'./{defect_types[0]}-vacancy/*/')
# creating a list of directories contained in input directory
list_dir = glob(wild_dir)

# search in every directory of the list
for dir in list_dir:
    path=os.path.dirname(dir) 
    # identifying last folder of path
    res = os.path.basename(path)
    reservoirs.append(res)
    

## SINGLE PLOTS ###############################################################

if get_single_plots:
    for res in reservoirs:
        
        plt.figure(figsize=(10,10))
    
        for item in defect_types:       
                
            data = np.loadtxt(f'./{item}-vacancy/{res}/{item}-vacancy-plot.dat', dtype = 'f')
            plt.plot(data[:,0],data[:,1], linewidth = 5 , label = f'$V_{{item}}$')
        
        plt.xlabel('Fermi level (eV)')
        plt.ylabel('Defect formation energy (eV)')
        
        # vertical lines
        plt.axvline(x = 0, color = 'k')
        plt.axvline(x = cbm , color = 'k')
        plt.hlines(0,xmin,xmax,colors='k',linestyles='dotted')
        # shaded areas
        plt.axvspan(xmin, 0, facecolor='k', alpha=0.2)
        plt.axvspan(cbm, xmax, facecolor='k', alpha=0.2)
        
        plt.grid()    
        plt.xlim(xmin,xmax)
        plt.title(f'{fun_xc}_{res}')
        plt.legend()    
     #   plt.show()
        plt.savefig(f'./{item}-vacancy/{res}/NN_{fun_xc}_vacancies_res{res}_E-form.pdf')
###############################################################################

# subplot grid initialization

if len(reservoirs) == 1:
    get_subplots = False
else:
    get_subplots = True
if len(reservoirs) == 2:
    nrows = 1
    ncolumns = 2
if len(reservoirs) == 3 or len(reservoirs)==4:
    nrows = 2
    ncolumns = 2    
if len(reservoirs) == 5 or len(reservoirs)==6:
    nrows = 2
    ncolumns = 3 
if len(reservoirs) == 7 or len(reservoirs)==8 or len(reservoirs)==9 :
    nrows = 3
    ncolumns = 3        
    
    
## SUBPLOTS ###################################################################
plt.figure(figsize=(30,20))

if get_subplots:    
    for res in reservoirs:
        
        sub_index = reservoirs.index(res) + 1
        plt.subplot(nrows,ncolumns,sub_index)        
    
        for item in defect_types:       
                
            data = np.loadtxt(f'./{item}-vacancy/{res}/{item}-vacancy-plot.dat', dtype = 'f')
            plt.plot(data[:,0],data[:,1], linewidth = 5 , label = '$V_{' + f'{item}' + '}$')
        
        plt.xlabel('Fermi level (eV)')
        plt.ylabel('Defect formation energy (eV)')
        
        # vertical lines
        plt.axvline(x = 0, color = 'k')
        plt.axvline(x = cbm , color = 'k')
        # shaded areas
        plt.axvspan(xmin, 0, facecolor='k', alpha=0.2)
        plt.axvspan(cbm, xmax, facecolor='k', alpha=0.2)
        plt.hlines(0,xmin,xmax,colors='k',linestyles='dashed')
 
        plt.grid()    
        plt.xlim(xmin,xmax)
        plt.ylim(ymin,ymax)
        plt.title(f'{res}')
        if res == reservoirs[len(reservoirs)-1]:
            plt.legend()    
     #   plt.show()
#        


#plt.show()
plt.savefig(f'./NN_{fun_xc}_vacancies_E-form.pdf')

