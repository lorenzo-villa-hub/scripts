import os
import sys
from glob import glob
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.io.vasp import Poscar
from pymatgen.analysis.eos import EOS ,EOSBase 

print('')
print(' Usage : eos_extract.py   path_to_EOS_calc ')
print('')
print('Script that extracts eos data and performs BM fit and plot')
print('')

input_dir = sys.argv[1]
#input_dir = '/nfshome/villa/local-data/NN_cubic/birch-murnaghan-2'

#system_name = os.path.basename(input_dir)
system_name = 'NN'

input_dir = (input_dir + '/')

#opening output file
file_name = f'{system_name}_eos.dat'
file_table = open(file_name,'w') 
# system name on file
file_table.write('# %s \n' % (system_name))
# legend on file
file_table.write('#V(A^3)  Etot(eV) \n')                 
   
#adding wild card to input directory
wild_dir = (input_dir + '*/')
#creating a list of directories contained in input directory
list_dir = glob(wild_dir)


for dir in list_dir:
    
    if dir != (input_dir + 'PBE_start/'):
    
        # reading vasprun.xml and POSCAR with pymatgen
        poscar = Poscar.from_file(dir + "POSCAR")
    
        structure = poscar.structure.as_dict()
        # getting lattice parameter
        a = structure['lattice']['a']
        # getting volume of cubic cell
        V = a*a*a
        
        # build path of vasprun.xml file
        path_vasprun = (dir + 'vasprun.xml')    
        # reading VASP OUTPUT
        vasprun = Vasprun(path_vasprun)
        etot = vasprun.final_energy
        
        file_table.write('%f  %f \n' %(V , etot))
    
file_table.close()    
    

print(f'Data printed in "{system_name}_eos.dat"')


import matplotlib.pyplot as plt
import numpy as np


import matplotlib 

matplotlib.rcParams.update({'font.size': 22})


data = np.loadtxt(f"{system_name}_eos.dat",'f')

V = data[:,0]
E = data[:,1] 

# making b-m fit and plot with pymatgen
eos = EOS(eos_name='birch_murnaghan')
eos_fit = eos.fit(V, E)
eos_fit.plot(width = 10, height = 10 , text = '', markersize = 15,  label= 'Birch-Murnaghan fit')  
plt.legend(loc=2, prop={'size': 20})
plt.title(f'{system_name}')
plt.tight_layout()
#plt.show()
plt.savefig(f'{system_name}_fit_eos.png')
print('')
print(f'Plot saved as "{system_name}_fit_eos.png"')

# getting fitted parameters
b0 = np.around(eos_fit.b0_GPa , decimals = 1)
b1 = np.around(eos_fit.b1 , decimals = 1)
v0 = np.around(eos_fit.v0 , decimals = 3)
a0 = np.around(np.cbrt(v0) , decimals = 3)
e0 = np.around(eos_fit.e0 , decimals = 3)

# writing fitted parameters
fit_file = open(f'{system_name}_fit_data.dat','w')

fit_file.write(f'Birch-Murnaghan fit of {system_name}\n')
fit_file.write('\n')
fit_file.write(f'V0 = {v0} A^3 , a0 = {a0} A\n')
fit_file.write(f'E0 = {e0} eV\n')
fit_file.write(f'B0 = {b0} GPa\n')
fit_file.write(f"B0' = {b1}\n")

fit_file.close()

print('')
print(f'Fitted data saved in "{system_name}_fit_data.dat"')
print('')
