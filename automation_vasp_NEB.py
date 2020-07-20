#!/usr/bin/env python

# -*- coding: utf-8 -*-
"""
Created on Tue Mar  3 16:56:38 2020

@author: villa
"""

from pynter.automations.core import CommandHandler, VaspAutomation
from pynter.automations.schemes import VaspSchemes, VaspNEBSchemes


# parse arguments
args = CommandHandler().vasp_args()

v = VaspAutomation(job_script_filename = args.job_script_filename, status_filename=args.status_filename, path=None)
s = VaspNEBSchemes(v,status=[], **args.__dict__)

if s.is_prevonvergence():
    if s.check_preconvergence_images():
        s.clean_NEB_dirs()
        s.copy_images_next_step_and_submit()
s.write_status()
    

    
