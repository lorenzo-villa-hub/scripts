#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  8 12:25:23 2021

@author: villa
"""


from pynter.data.jobs import Job
from pynter.vasp.jobs import VaspJob, VaspNEBJob
import argparse as ap


class RunJob:
          
    def args(self):
        """
        Add and parse arguments from command line
        """
        
        parser = ap.ArgumentParser()
        
        parser.add_argument('-j','--jobclass',help='Job class, default is VaspJob',required=False,default='VaspJob',type=str,metavar='',dest='jobclass')
        parser.add_argument('-s','--sync',help='sync to hpc',required=False,default=False,action='store_true',dest='sync')
        parser.add_argument('-f','--filename',help='job_script_filename',required=False,default=None,type=str,metavar='',dest='filename')
        parser.add_argument('-R','--run',help='Run job',required=False,default=False,action='store_true',dest='run')
        
        args = parser.parse_args()
        
        # update settings
        for key,value in args.__dict__.items():
            setattr(self,key,value)
                
        return
    
    def run_job(self):   
        cls = globals()[self.jobclass]
        j = cls.from_directory('.',job_script_filename=self.filename,load_outputs=False)
        if self.run:
            j.run_job(write_input=False,sync=self.sync)
        return
 
    def sync_to_hpc(self):
        cls = globals()[self.jobclass]
        j = cls.from_directory('.',job_script_filename=self.filename,load_outputs=False)
        if self.sync:
           j.sync_to_hpc()
           return
    

if __name__ == '__main__':
    r = RunJob()
    r.args()
    if r.run:
        r.run_job()
    elif r.sync:
        r.sync_to_hpc()
