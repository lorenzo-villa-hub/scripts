import matplotlib
import matplotlib.pyplot as plt
import numpy as np

plt.figure(figsize=(10,10))
matplotlib.rcParams.update({'font.size': 28})

data1 = np.loadtxt('O-vacancy-E-form.dat', 'f')
data2 = np.loadtxt('corrected_E-form.dat', 'f')

plt.plot(data1[:,0] , data1[:,1] , 'o', markersize = 20 , label = 'no corrections')
plt.plot(data2[:,0] , data2[:,1] , 'o',markersize = 20 , label = 'with corrections')

plt.xlabel('Charge state')
plt.ylabel('Formation Energy (eV)')
plt.title('O-vacancy')
plt.xticks(np.arange(-2,3,1))
plt.yticks(np.arange(-1, 9 , 1.0))

plt.grid(True)
plt.legend()
plt.savefig('corr-E-form.png')


