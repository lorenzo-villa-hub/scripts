#!/nfshome/villa/anaconda3/bin/python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec
from matplotlib.pyplot import figure


import matplotlib
matplotlib.rcParams.update({'font.size': 22})


#
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter

vaspout = Vasprun("./vasprun.xml")

# give range for y axis
ymin = -7
ymax = 6


# path for location of DOS file
dos_path = '.'
dos_path += '/'

# give elements in list
type_elements = ['Na','Nb','O']

# give a list of orbitals you want to plot for each element : 'element_orbital'
list_dos_plot = ['Na_s','Nb_s','Nb_d','O_p']


for el in type_elements:

    
    data = np.loadtxt(dos_path + f'DOS_{el}.dat', dtype='f')

    energies = data[:,0]
    s_dos = data[:,1]
    p_dos = data[:,2]
    d_dos = data[:,3]
    
    # check if orbital is in plotting list
    if f'{el}_s' in list_dos_plot:
        plt.plot(s_dos, energies, label = f'{el}' + '($\it{s}$)')
    if f'{el}_p' in list_dos_plot:
        plt.plot(p_dos, energies, label = f'{el}' + '($\it{p}$)')
    if f'{el}_d' in list_dos_plot:
        plt.plot(d_dos, energies, label = f'{el}' + '($\it{d}$)')
    
           
data_tot = np.loadtxt(dos_path + 'DOS_total.dat', dtype = 'f')
energies = data_tot[:,0]
tot_dos = data_tot[:,1]   


#plt.plot(tot_dos, energies, label = 'total')
plt.fill_betweenx(energies,tot_dos, alpha = 0.25, color = 'k')

plt.xlabel('DOS (a.u.)')
plt.ylabel('E - E$_{F}$ (eV)')
# axes
ax = plt.gca()
#plt.yticks([])
# legend on right side of plot
#ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))

plt.ylim(ymin,ymax)
plt.xlim(0,200)


plt.legend()
#plt.show()

plt.savefig('dos_custom.png')
