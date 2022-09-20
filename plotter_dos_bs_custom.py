#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from pynter.vasp.pmg_electronic_structure_plotter import BSPlotter , BSDOSPlotter, DosPlotter
from pymatgen.electronic_structure.dos import FermiDos, CompleteDos
import argparse
import os
import json
import sys
#import matplotlib.pyplot as plt

os.chdir('/nfshome/villa/local-data/NN_cubic/DOS-BS/PBE/2-BS')

parser = argparse.ArgumentParser()

parser.add_argument('-p','--path-to-dos',help='Path to Dos object saved as json or path to where vasprun for dos is stored',required=False,type=str,default=None,metavar='',dest='path_to_dos')    
parser.add_argument('-s','--save',help='Save fig as pdf',required=False,default=False,action='store_true',dest='savefig')   
parser.add_argument('-hy','--hybrid-mode',help='Force hybrid mode for BS',required=False,default=False,action='store_true',dest='hybrid')   
parser.add_argument('-y','--ylim',help='Range for y-axis',required=False,default=None,nargs='+',type=float,metavar='',dest='ylim')
parser.add_argument('-t','--title',help='Title of the plot',required=False,type=str,default=None,metavar='',dest='title') 

a = parser.parse_args()


if not a.ylim:
    a.ylim = (-10,10)
    
get_dos = False
if a.path_to_dos:
    if '.json' in a.path_to_dos:
        with open(a.path_to_dos) as f:
            dos = CompleteDos.from_dict(json.load(f))
    elif a.path_to_dos:
        try:
            dos = Vasprun(os.path.join(a.path_to_dos,"vasprun.xml")).complete_dos
        except:
            raise ValueError(f'Reading of dos from vasprun.xml in {a.path_to_dos} failed')
else:
    get_dos = True

v = Vasprun('vasprun.xml')

if get_dos:
    dos = v.complete_dos
efermi = dos.efermi
bs = v.get_band_structure(line_mode=True,force_hybrid_mode=a.hybrid,efermi=efermi)


# PLOT BS

plt = BSPlotter(bs).get_plot(ylim=a.ylim,get_subplot=True)
ax1 = plt.gca()
plt.xticks(fontsize=25)
plt.yticks(fontsize=25)
    

# PLOT DOS

partial_dos = dos.get_spd_dos()
dos_plotter = DosPlotter()
dos_plotter.add_dos('total',dos)
for orbital in partial_dos:
    dos_plotter.add_dos(orbital,partial_dos[orbital])
plt = dos_plotter.get_plot(xlim=a.ylim,get_subplot=True)    

# modify pymatgen output to flip graph
ax = plt.gca()
ymax = 0
fermi_lines = []
for i in range(0,len(ax.lines)):
    x = ax.lines[i].get_ydata()
    y = ax.lines[i].get_xdata()
    for j in range(0,len(x)):
        if x[j] > a.ylim[0] and x[j] < a.ylim[1]:
            if x[j] > ymax:
                ymax = x[j]
    ax.lines[i].set_xdata(x)
    ax.lines[i].set_ydata(y)
    if ax.lines[i].get_linestyle() == '--':
        fermi_lines.append(ax.lines[i])
        
ax.lines = [l for l in ax.lines if l not in fermi_lines] 
xlim = ax.get_ylim()
ylim = ax.get_xlim()
ax.set_xlim()
ax.set_ylim()
ax.set_xlim(0,ymax)
ax.set_ylim(a.ylim)
ax.set_xlabel('Density of states',size=25)
ax.set_yticks([])
ax.set_ylabel(None)
plt.xticks(fontsize=25)

fig = plt.gcf()
fig.tight_layout()


if a.savefig:
    plt.savefig('DOS-BS.pdf')
else:
    plt.show()

