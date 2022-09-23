#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from pynter.vasp.pmg_electronic_structure_plotter import BSPlotter , BSDOSPlotter, DosPlotter
from pymatgen.electronic_structure.dos import FermiDos, CompleteDos
import argparse
import os
import json
import sys
#import matplotlib.pyplot as plt

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

#BSDOS plotter SECTION


efermi = dos.efermi
gap = dos.get_gap()
ylim = a.ylim
vb_range = -1*ylim[0] if ylim else 4  # adjust for nonsense args in BSDOSPlotter
cb_range = ylim[1] - gap if ylim else 4  # adjust for nonsense args in BSDOSPlotter
bs = v.get_band_structure(line_mode=True,force_hybrid_mode=a.hybrid,efermi=efermi)

plt = BSDOSPlotter(bs_projection=None, dos_projection='elements', bs_legend=None, 
                    vb_energy_range=vb_range, cb_energy_range=cb_range).get_plot(bs,dos)
ax = plt.gcf().get_axes()[0]
ax.set_ylabel('$E - E_F$ (eV)')

if a.savefig:
    plt.savefig('DOS-BS.pdf')
else:
    plt.show()

