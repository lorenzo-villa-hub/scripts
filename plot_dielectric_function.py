#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from pymatgen.io.vasp.outputs import Vasprun

plt.figure(figsize=(15,15))

import matplotlib
matplotlib.rcParams.update({'font.size': 22})

vasprun = Vasprun("./vasprun.xml")
epsilon = vasprun.dielectric


w = np.zeros(len(epsilon[0]))

for i in range(0,len(epsilon[0])):
    w[i] = epsilon[0][i]
    
e1 = np.zeros(len(epsilon[0]))

for i in range(0,len(epsilon[1])):
    e1[i] = epsilon[1][i][0]
        
e2 = np.zeros(len(epsilon[2]))


for i in range(0,len(epsilon[2])):
    e2[i] = epsilon[2][i][0]    


plt.plot(w, e1, label = 'real part')
plt.plot(w, e2, label = 'imaginary part')

plt.xlim(0,10)
plt.xlabel('Energy (eV)')
plt.ylabel('Dielectric function')
plt.title(system_name)
plt.grid()
plt.legend()

#plt.show()

plt.savefig(f"dielectric_function.pdf")
plt.savefig(f"dielectric_function.png")

