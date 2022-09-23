#!/usr/bin/env python
"""
Spyder Editor

This is a temporary script file.
"""

import os
from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import DosPlotter
import argparse

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('-sg','--sigma',help='Standard deviation for Gaussian smearing of DOS',required=False,type=float,default=None,metavar='',dest='sigma')    
parser.add_argument('-s','--save',help='Save fig as pdf',required=False,default=False,action='store_true',dest='savefig')   
parser.add_argument('-st','--stack',help='Stack DOS',required=False,default=False,action='store_true',dest='stack') 
parser.add_argument('-y','--ylim',help='Range for y-axis',required=False,default=None,nargs='+',type=float,metavar='',dest='ylim')
parser.add_argument('-x','--xlim',help='Range for x-axis',required=False,default=None,nargs='+',type=float,metavar='',dest='xlim')
parser.add_argument('-p','--projection',help='Projection of DOS, "elements", "orbitals" or "None"',required=False,type=str,default='elements',metavar='',dest='dos_projection')    
parser.add_argument('-t','--title',help='Title of the plot',required=False,type=str,default=None,metavar='',dest='title') 

a = parser.parse_args()

xlim = a.xlim if a.xlim else (-10,10)
complete_dos = Vasprun('vasprun.xml').complete_dos

plotter = DosPlotter(stack=a.stack,sigma=a.sigma)
plotter.add_dos('Total',complete_dos)

if a.dos_projection == 'orbitals':
    dos_dict = complete_dos.get_spd_dos()
elif a.dos_projection == 'elements':
    dos_dict = complete_dos.get_element_dos()
for k in dos_dict:
    if not a.dos_projection=='None':   
        plotter.add_dos(k,dos_dict[k])
plt = plotter.get_plot(xlim=xlim,ylim=a.ylim)
plt.show()
