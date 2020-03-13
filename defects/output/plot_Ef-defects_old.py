
import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(10,10))
import matplotlib 
#matplotlib.rc('xtick', labelsize=20) 
#matplotlib.rc('ytick', labelsize=20)
matplotlib.rcParams.update({'font.size': 22})


defect_types = ['Ag','Nb','O']

xmin = -0.5
xmax = 1.7
cbm = 1.318




for item in defect_types:
    
    data = np.loadtxt(f'./{item}-vacancy/{item}-vacancy-plot_corr.dat', dtype = 'f')
    plt.plot(data[:,0],data[:,1], linewidth = 5 , label = f'{item}-vacancy')

plt.xlabel('Fermi level (eV)')
plt.ylabel('Defect formation energy (eV)')

# vertical lines
plt.axvline(x = 0, color = 'k')
plt.axvline(x = cbm , color = 'k')
# shaded areas
plt.axvspan(xmin, 0, facecolor='k', alpha=0.2)
plt.axvspan(cbm, xmax, facecolor='k', alpha=0.2)

plt.grid()    
plt.xlim(xmin,xmax)
plt.title('AgNbO3')
plt.legend()    
#plt.show()
plt.savefig('AN_PBE_defects_E-form_corr.png')
