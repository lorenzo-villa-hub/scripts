import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(18,10))
import matplotlib 
#matplotlib.rc('xtick', labelsize=20) 
#matplotlib.rc('ytick', labelsize=20)
matplotlib.rcParams.update({'font.size': 22})


data = np.loadtxt('U_tuning-a-Eg.dat','f')

U = data[:,0]
a = data[:,1]
Eg = data[:,2]


# plot U vs a
plt.subplot(1,2,1)

plt.xlabel('Coulomb parameter (eV)')
plt.ylabel('Lattice parameter ($\AA$)')
plt.grid()
plt.title('NaNbO3')
plt.scatter(U,a, marker = (4,0), s = 500)

# plot U vs Eg
plt.subplot(1,2,2)
plt.xlabel('Coulomb parameter (eV)')
plt.ylabel('Energy gap (eV)')
plt.grid()
plt.scatter(U,Eg, marker = (4,0), s = 500, color = 'r')
plt.title('NaNbO3')

# save plot
plt.savefig('NN_U_tuning.png')