#!/usr/bin/env python

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter , BSDOSPlotter
from pymatgen.electronic_structure.dos import FermiDos, CompleteDos
import argparse
import os
import json
import sys

parser = argparse.ArgumentParser()

parser.add_argument('-p','--path-to-dos',help='Path to Dos object saved as json or path to where vasprun for dos is stored',required=False,type=str,default=None,metavar='',dest='path_to_dos')    
parser.add_argument('-s','--savefig',help='Save fig as pdf',required=False,default=False,action='store_true',dest='savefig')   
parser.add_argument('-hy','--hybrid-mode',help='Force hybrid mode for BS',required=False,default=False,action='store_true',dest='hybrid')   
parser.add_argument('-y','--ylim',help='Range for y-axis',required=False,default=None,nargs='+',type=int,metavar='',dest='ylim')

args = parser.parse_args()

get_dos = False
if args.path_to_dos:
    if '.json' in args.path_to_dos:
        with open(args.path_to_dos) as f:
            dos = CompleteDos.from_dict(json.load(f))
    elif args.path_to_dos:
        try:
            dos = Vasprun(os.path.join(args.path_to_dos,"vasprun.xml")).complete_dos
        except:
            raise ValueError(f'Reading of dos from vasprun.xml in {args.path_to_dos} failed')
else:
    get_dos = True

v = Vasprun('vasprun.xml')
if get_dos:
    dos = v.complete_dos

efermi = dos.efermi
gap = dos.get_gap()
ylim = args.ylim
vb_range = -1*ylim[0] if ylim else 4  # adjust for nonsense args in BSDOSPlotter
cb_range = ylim[1] - gap if ylim else 4  # adjust for nonsense args in BSDOSPlotter
bs = v.get_band_structure(line_mode=True,force_hybrid_mode=args.hybrid,efermi=efermi)

plt = BSDOSPlotter(bs_projection=None, dos_projection='elements', bs_legend=None, 
                   vb_energy_range=vb_range, cb_energy_range=cb_range).get_plot(bs,dos)
ax = plt.gcf().get_axes()[0]
ax.set_ylabel('$E - E_F$ (eV)')

if args.savefig:
    plt.savefig('DOS-BS.pdf')
else:
    plt.show()

