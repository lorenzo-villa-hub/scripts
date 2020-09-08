import matplotlib
import matplotlib.pyplot as plt
import numpy as np


data = []

for i in range(2,5):
    
    path = '/nfshome/villa/local-data/NN_cubic/E-form-supercells/'
    data_path = path + f'{i}-supercell/'
    data_file = data_path + 'O-vacancy-E-form.dat'
    data.append(np.loadtxt(data_file, 'f'))
                     
    plt.plot(data[i-2][:,0],data[i-2][:,1] , 'o', markersize = 10, label = f'{i}x{i}x{i}')
    plt.xlabel('Charge state')
    plt.ylabel('Formation Energy [eV]')
plt.xticks(np.arange(-2,3,1))
plt.yticks(np.arange(-1, 10 , 1.0))
plt.grid(True)    
plt.legend()
plt.savefig('E-form-supercells.png')    

e_diff = open('E-difference.dat','w')
e_diff.write('#q     E(s2-s4)     E(s3-s4) \n')
for i in range(0,5):
    q = data[0][i,0]
    diff_2_4 = data[0][i,1] - data[2][i,1]
    diff_3_4 = data[1][i,1] - data[2][i,1]
    e_diff.write('%f %f %f \n'  %(q , diff_2_4, diff_3_4))
