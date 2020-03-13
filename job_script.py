#!/usr/bin/env python

import os
import argparse as ap

class ScriptHandler:
    
    def __init__(self, **kwargs):
        
        default_settings = {
                    'project_id': '01136',
                    'name': 'no_name',
                    'array_size': None,
                    'email':'villa@mm.tu-darmstadt.de',
                    'nodes': 4,
                    'cores_per_node': 24,
                    'timelimit': '24:00:00',
                    'memory_per_cpu':2400,
                    'processor':'avx2',
                    'modules':['intel/2019.2','intel/2019.3','intelmpi/2019.3','fftw/3.3.8'],
                    'path_exe':'/home/lv51dypu/vasp-5-3-3',
                    'add_stop_array':True,
                    'add_automation':'automized_next_step_error_check.py',
                    'add_lines_header':None,
                    'add_lines_body':None,
                    'filename':'job_vasp.sh'
                     }
        
        for key,value in default_settings.items():
            setattr(self,key,value)
        
        for key, value in kwargs.items():
            if key in default_settings:
                setattr(self,key,value)
            else:
                raise Exception('"%s" is not a possible argument \nPossible arguments are: %s' %(key, self.settings.keys()))
  
              
    @property
    def settings(self):
        return self.__dict__
    
    
    def args(self):
        """
        Add and parse arguments from command line
        """
        
        parser = ap.ArgumentParser()
        
        parser.add_argument('-A','--project',help='Project ID, default is 01136',required=False,default=self.project_id,type=str,metavar='',dest='project_id')
        parser.add_argument('-n','--name',help='Job name',required=False,default=self.name,type=str,metavar='',dest='name')
        parser.add_argument('-a','--array',help='Size of job array',required=False,default=self.array_size,type=int,metavar='',dest='array_size')
        parser.add_argument('-e','--email',help='Email address for job notification',required=False,default=self.email,type=str,metavar='',dest='email')
        parser.add_argument('-N','--nodes',help='Number of nodes, default is 4',required=False,default=self.nodes,type=int,metavar='',dest='nodes')
        parser.add_argument('-c','--cores-per-node',help='Number of cores per node, default is 24',required=False,default=self.cores_per_node,type=int,metavar='',dest='cores_per_node')
        parser.add_argument('-t','--timelimit',help='Timelimit, default is 24:00:00',required=False,default=self.timelimit,type=str,metavar='',dest='timelimit')
        parser.add_argument('-M','--memory-per-cpu',help='Memory per cpu, default is 2400',required=False,default=self.memory_per_cpu,type=int,metavar='',dest='memory_per_cpu')
        parser.add_argument('-C','--processor',help='(avx or avx2, default is avx2)',required=False,default=self.processor,type=str,metavar='',dest='processor')
        parser.add_argument('-ml','--modules',action='append',help="Modules to load, default are 'intel/2019.2','intel/2019.3','intelmpi/2019.3','fftw/3.3.8'" ,required=False,default=self.modules,type=str,metavar='',dest='modules')
        parser.add_argument('-x','--exe',help='Path to executable, default is "/home/lv51dypu/vasp-5-3-3"',required=False,default=self.path_exe,type=str,metavar='',dest='path_exe')
        parser.add_argument('-s','--stop-array',action='store_true',help='Add lines to stop array jobs when converged, default is False',required=False,default=False,dest='add_stop_array')
        parser.add_argument('-S','--automation',help='Script with automation to add',required=False,default=self.add_automation,type=str,metavar='',dest='add_automation')
        parser.add_argument('-H','--header',action='append',help='Add line to header part of script',required=False,default=self.add_lines_header,type=str,metavar='',dest='add_lines_header')
        parser.add_argument('-B','--body',action='append',help='Add line to body part of script',required=False,default=self.add_lines_body,type=str,metavar='',dest='add_lines_body')
        parser.add_argument('-f','--filename',help='File name, default is "job_vasp.sh"',required=False,default=self.filename,type=str,metavar='',dest='filename')
        
        args = parser.parse_args()
        
        # update settings
        for key,value in args.__dict__.items():
            setattr(self,key,value)
                
        return
            
    
    def write_script(self,path=None):
        """
        Write job script 
        
        Parameters
        ----------
        path : (str), optional
            Path to write job script to. The default is None. If None work dir is used.
        """
        
        self.write_script_header(path=path)
        self.write_script_body(path=path)
        
        return
    

    def write_script_body(self,path=None):
        """
        Write body part of the job script (part after #SBATCH commands) 
        
        Parameters
        ----------
        path : (str), optional
            Path to write job script to. The default is None. If None work dir is used.
        """
        
        complete_path = os.path.join(path,self.filename) if path else self.filename        
        with open(complete_path,'a') as f:
            
            if self.array_size:
                f.write('\n')
                f.write('if [ -f CONTCAR ]\n')
                f.write('then\n')
                f.write('    cp CONTCAR POSCAR\n') # KEEP THE TAB !)
                f.write('fi\n')
            
            f.write('\n')
            f.write('srun %s\n' %self.path_exe)

            automation_written = False
            if self.array_size:
                if self.add_stop_array:
                    f.write('\n')
                    f.write('convergence.py > convergence.txt\n')
                    f.write("if  grep -q 'Electronic convergence: True' convergence.txt  = true  && grep -q 'Ionic convergence: True' convergence.txt  = true; then\n")
                    if self.add_automation:
                        f.write('    %s\n' %self.add_automation) # KEEP THE TAB!
                        automation_written = True
                    f.write('    scancel ${SLURM_ARRAY_JOB_ID}_*\n') #KEEP THE TAB!
                    f.write('fi\n')
            if self.add_automation and automation_written is False:
                f.write('\n')
                f.write('%s\n' %self.add_automation)

            if self.add_lines_body:
                f.writelines('\n'.join([l for l in self.add_lines_body]))

        return
                                

    def write_script_header(self,path=None):
        """
        Write header part of the job script (part with #SBATCH commands and module loads) 
        
        Parameters
        ----------
        path : (str), optional
            Path to write job script to. The default is None. If None work dir is used.
        """
        
        complete_path = os.path.join(path,self.filename) if path else self.filename        
        with open(complete_path,'w') as f:
            
            f.write('#!/bin/sh\n')
            f.write('#SBATCH -A project%s\n' %self.project_id)
            f.write('#SBATCH --job-name=%s\n' %self.name)
            if self.array_size:
                f.write('#SBATCH --array=1-%i%%1\n' %self.array_size)
            f.write('#SBATCH --mail-user=%s\n' %self.email)
            f.write('#SBATCH --mail-type=ALL\n')
            f.write('#SBATCH --nodes=%i\n' %self.nodes)
            f.write('#SBATCH --ntasks-per-node=%i\n' %self.cores_per_node)
            f.write('#SBATCH --cpus-per-task=1\n')
            f.write('#SBATCH --output=out.%j\n')
            f.write('#SBATCH --error=err.%j\n')
            f.write('#SBATCH --time=%s\n' %self.timelimit)
            f.write('#SBATCH -p deflt\n')
            f.write('#SBATCH --exclusive\n')
            f.write('#SBATCH --mem-per-cpu=%i\n' %self.memory_per_cpu)
            f.write('#SBATCH -C %s\n' %self.processor)
            f.writelines([' '.join(['ml', m , '\n']) for m in self.modules])
            if self.add_lines_header:
                f.writelines('\n'.join([l for l in self.add_lines_header]))

        return         
        
# part to execute if script is used directly        
if __name__ == '__main__':
    
    s = ScriptHandler()
    # getting arguments
    s.args()
    # write file
    s.write_script()
