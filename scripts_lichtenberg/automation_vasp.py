#!/usr/bin/env python

# -*- coding: utf-8 -*-
"""
Created on Tue Mar  3 16:56:38 2020

@author: villa
"""

from my_functions.automations.core import CommandHandler, VaspAutomation
from my_functions.automations.schemes import VaspSchemes


# parse arguments
args = CommandHandler().vasp_args()

v = VaspAutomation(job_script_filename = args.job_script_filename, status_filename=args.status_filename, path=None)
s = VaspSchemes(v,status=[], **args.__dict__)

conv_el, conv_ionic = s.check_convergence()

if conv_el and conv_ionic:
    s.next_step_relaxation_schemes()
elif s.error_check:
    s.resubmit_if_step_limits_reached()
s.write_status()
    

    
