#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on Wed Feb 19 17:44:10 2020

@author: villa
"""

from pymatgen.io.vasp.outputs import Vasprun
from pymatgen.electronic_structure.plotter import BSPlotter
import argparse

parser = argparse.ArgumentParser()

parser.add_argument('-k','--kpoints-filename',help='KPOINTS filename',required=False,type=str,default=None,metavar='',dest='kpoints_filename')    
parser.add_argument('-s','--save',help='Save fig as pdf',required=False,default=False,action='store_true',dest='savefig')   
parser.add_argument('-hy','--hybrid-mode',help='Force hybrid mode for BS',required=False,default=False,action='store_true',dest='force_hybrid_mode') 
parser.add_argument('-l','--line-mode',help='Line mode for BS',required=False,default=True,action='store_true',dest='line_mode')   
parser.add_argument('-y','--ylim',help='Range for y-axis',required=False,default=None,nargs='+',type=float,metavar='',dest='ylim')
parser.add_argument('-sm','--smooth',help='Smooth band structure',required=False,default=False,action='store_true',dest='smooth')  
parser.add_argument('-t','--title',help='Title of the plot',required=False,type=str,default=None,metavar='',dest='title') 

a = parser.parse_args()

v = Vasprun("vasprun.xml")
bs = v.get_band_structure(kpoints_filename=a.kpoints_filename,line_mode=a.line_mode,force_hybrid_mode=a.force_hybrid_mode)
plt = BSPlotter(bs).get_plot(ylim=a.ylim)
plt.gca().get_legend().remove()
if a.title:
    plt.title(a.title,fontdict={'fontsize':25})
if a.savefig:
    plt.savefig('BS.pdf')
else:
    plt.show()
