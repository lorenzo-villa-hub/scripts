#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  8 12:25:23 2021

@author: villa
"""


from pynter.data.jobs import Job
import argparse as ap


class RunJob:
          
    def args(self):
        """
        Add and parse arguments from command line
        """
        
        parser = ap.ArgumentParser()
        
        parser.add_argument('-j','--jobclass',help='Job class, default is VaspJob',required=False,default='VaspJob',type=str,metavar='',dest='jobclass')
        parser.add_argument('-s','--sync',help='sync to hpc',required=False,default=True,type=str,metavar='',dest='sync')
        parser.add_argument('-f','--filename',help='job_script_filename',required=False,default=None,type=str,metavar='',dest='filename')
        
        args = parser.parse_args()
        
        # update settings
        for key,value in args.__dict__.items():
            setattr(self,key,value)
                
        return

    def find_jobclass(self):
        for c in Job.__subclasses__():
            if c.__name__ == self.jobclass:
                return c
    
    def run(self):   
        jobclass = self.find_jobclass()
        j = jobclass.from_directory('.',job_script_filename=self.filename)
        j.run_job(write_input=False,sync=self.sync)
        return
    

if __name__ == '__main__':
    r = RunJob()
    r.args()
    r.run()