import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(12,12))
import matplotlib 
#matplotlib.rc('xtick', labelsize=20) 
#matplotlib.rc('ytick', labelsize=20)
matplotlib.rcParams.update({'font.size': 28})

functionals = ['PBE','PBE+U','SCAN','HSE06']


data = np.genfromtxt('Eg-a-table.dat')

a = data[0:len(data)-1,1]
Eg = data[0:len(data)-1,2]


x = np.arange(len(data)-1)
plt.bar(x, height= a)
plt.xticks(x, functionals)
plt.hlines(3.96 , -0.5, len(data)+0.5, linestyles = 'dashed', label = 'Exp')
plt.xlim(-0.5,len(data)-1-0.5)
plt.ylim(3.75,4.1)
plt.ylabel('Lattice parameter ($\AA$)')
plt.legend()
ax = plt.axes()        
ax.yaxis.grid() # horizontal lines
plt.savefig('AN_hbar_plot-a.png')
