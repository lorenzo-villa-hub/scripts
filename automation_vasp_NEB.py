#!/usr/bin/env python

# -*- coding: utf-8 -*-
"""
Created on Tue Mar  3 16:56:38 2020

@author: villa
"""

from pynter.automations.core import CommandHandler
from pynter.automations.vasp import NEBSchemes


# parse arguments
args = CommandHandler().vasp_args()

s = NEBSchemes(path=None,status=[], **args.__dict__)

if s.is_preconvergence():
    if s.check_preconvergence_images():
        s.copy_images_next_step_and_submit()
else:
    if s.is_NEB_job_finished():
        s.copy_images_next_step_and_submit()
s.write_status()
    

    
