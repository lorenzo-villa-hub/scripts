
import numpy as np
import matplotlib.pyplot as plt
# data structure -> charge,E(2s),E(3s),E(4s)


plt.rcParams.update({'font.size': 18})

xlim = 0, 3

# data for CORRECTED  energies
data = np.zeros([5,4])
# data for UNCORRECTED energies
data_uncorr = np.zeros([5,4])
for i in range(2,5):
    single_data = np.loadtxt(f'./{i}-supercell/{i}s_corrected_E-form.dat','f')
    data[:,0] = single_data[:,0]
    data[:,i-1] = single_data[:,1]
    
    single_data_uncorr = np.loadtxt(f'./{i}-supercell/{i}s_uncorrected_E-form.dat','f')
    data_uncorr[:,0] = single_data_uncorr[:,0]
    data_uncorr[:,i-1] = single_data_uncorr[:,1]

fig = plt.figure(figsize=(10,10))
ax1 = fig.add_subplot(111)
ax2 = ax1.twiny()

# building plots    
for i in range(0,len(data)):
    
    # CORRECTED ENERGIES
    # concentration values
    x = [1/320,1/135,1/40]
    x = np.asarray(x)*100
    ax2.set_xticks(x)
    ax2.set_xticklabels(['320','135','40'])
    # energies
    y = np.array([data[i,3],data[i,2],data[i,1]])
    ax1.scatter(x,y,105,c='k')   #  ,label=f'{data[i,0]}')
    
    # LINEAR FITTING
    # list with values of m and q of linear fitting
    coef = np.polyfit(x,y,1)
    # function that gives y of a given x from values of m and q
    poly1d_fn = np.poly1d(coef)
    # cunstructing points for line plot
    x_line = np.array([xlim[0],xlim[1]])
    y_line = np.array( [poly1d_fn(0), poly1d_fn(x[2])])
    ax1.plot(x_line,y_line,label=f'q={int(data[i,0])}')
    
    # UNCORRECTED ENERGIES
    y_uncorr = np.array([data_uncorr[i,3],data_uncorr[i,2],data_uncorr[i,1]])
    ax1.scatter(x,y_uncorr,105,c='none',edgecolors='k') #  ,label=f'{data[i,0]}')


ax1.set_xlim(xlim[0],xlim[1])
ax2.set_xlim(ax1.get_xlim())
ax1.set_xlabel('Defect concentration (%)')
ax1.set_ylabel('Oxygen vacancy formation energy (eV)')
ax2.set_xlabel('Number of atoms in supercell')
ax1.grid()
ax1.legend()   


plt.savefig('finite_size_scaling.pdf')