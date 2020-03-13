#!/nfshome/villa/anaconda3/bin/python
# script to generate files for DOS and plot it

import numpy as np
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen import Structure
from pymatgen.io.vasp import Poscar


#from pymatgen.electronic_structure.plotter import DosPlotter
#from pymatgen.electronic_structure.dos import Dos , CompleteDos

# reading POSCAR 
poscar = Poscar.from_file("POSCAR")
structure = poscar.structure.as_dict()

# getting dict of elements in POSCAR
number_elements = poscar.natoms
type_elements = poscar.site_symbols
elements = {}
for i in type_elements:
    index_list = type_elements.index(i)
    elements.update({i : number_elements[index_list]})
#

# reading VASP OUTPUT
vasprun = Vasprun("./vasprun.xml")

dos_dict = vasprun.complete_dos.as_dict()
efermi = vasprun.complete_dos.efermi
energies = vasprun.tdos.energies
energies = energies - efermi
total_dos = dos_dict['densities']['1']

pdos = dos_dict['pdos']
pdos_index = 0
total_elements = 0
s_dos = np.zeros(len(energies))
p_dos = np.zeros(len(energies))
d_dos = np.zeros(len(energies))
tot_dos = np.zeros(len(energies))

write_index = {}

for el in elements:
    total_elements += elements[el]
    write_index[el] = total_elements - 1

cycle = 0


# summing PDOS
for el in elements:
    
    dos_file = open(f'DOS_{el}.dat','w')
    dos_file.write(f'#Energy  {el}_s  {el}_p  {el}_d  total \n')
    total_dos_file = open('DOS_total.dat','w')
    total_dos_file.write('#Energy  total_DOS \n')
    
    for nel in range(elements[el]):           
        for i in range(len(energies)):
            # DOS of s orbitals
            s_dos[i] = s_dos[i] + pdos[pdos_index]['s']['densities']['1'][i]
            # sum of DOS of p orbitals
            p_dos[i] = p_dos[i] + pdos[pdos_index]['px']['densities']['1'][i]
            p_dos[i] = p_dos[i] + pdos[pdos_index]['py']['densities']['1'][i]
            p_dos[i] = p_dos[i] + pdos[pdos_index]['pz']['densities']['1'][i]
            # sum of DOS of d orbitals
            d_dos[i] = d_dos[i] + pdos[pdos_index]['dx2']['densities']['1'][i]
            d_dos[i] = d_dos[i] + pdos[pdos_index]['dxy']['densities']['1'][i]
            d_dos[i] = d_dos[i] + pdos[pdos_index]['dxz']['densities']['1'][i]
            d_dos[i] = d_dos[i] + pdos[pdos_index]['dyz']['densities']['1'][i]
            d_dos[i] = d_dos[i] + pdos[pdos_index]['dz2']['densities']['1'][i]
            
            tot_dos[i] = s_dos[i] + p_dos[i] + d_dos[i]
                        
            if (pdos_index) == write_index[el]:
                dos_file.write('%f %f %f %f %f \n' %(energies[i] , s_dos[i] , p_dos[i] , d_dos[i] , tot_dos[i]))
                total_dos_file.write('%f %f \n' %(energies[i] , total_dos[i]))
                                                                       
        if (pdos_index) == write_index[el]:
            s_dos = np.zeros(len(energies))
            p_dos = np.zeros(len(energies))
            d_dos = np.zeros(len(energies))
            
        pdos_index = pdos_index + 1     
        
        
    dos_file.close()              

#############################################################################           
# plotting DOS 
        
import numpy as np
import matplotlib.pyplot as plt


plt.figure(figsize=(15,10))
plt.rcParams.update({'font.size': 20})

for el in type_elements:

    data = np.loadtxt(f'DOS_{el}.dat', dtype='f')

    energies = data[:,0]
    s_dos = data[:,1]
    p_dos = data[:,2]
    d_dos = data[:,3]
    
    plt.plot(energies, s_dos, label = f'{el}_s')
    plt.plot(energies, p_dos, label = f'{el}_p')
    plt.plot(energies, d_dos, label = f'{el}_d')
    
    plt.xlabel('Energy (eV)')
    plt.ylabel('DOS (a.u.)')
        
data_tot = np.loadtxt('DOS_total.dat', dtype = 'f')
energies = data_tot[:,0]
tot_dos = data_tot[:,1]   


#plt.plot(energies, tot_dos, label = 'total')
plt.fill_between(energies,tot_dos, alpha = 0.15, color = 'k')

plt.xlim(-12,12)
plt.ylim(0,18)


 


plt.legend()
#plt.show()
plot_name = ''
for el in elements:
    if elements[el] == 1:
        plot_name += el
    else:
        plot_name += el + str(elements[el])
    

plt.savefig(f'{plot_name}_DOS.png')
