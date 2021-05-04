#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May  4 16:42:35 2021

@author: lorenzo
"""
import re
import os
import argparse as ap
from glob import glob


class ModifyFiles:
    
    def __init__(self,wildpath=None,files=None,exclude=None,backup=False,str_to_modify = {}, lines_to_add=[],
                 check_added_lines = True, lines_to_remove = [], print_file=False):
            if wildpath:
                self.wildpath = wildpath
                self.files = glob(os.path.normpath(wildpath))
            elif files:
                self.files = files
            self.files = files
            self.exclude = exclude
            self.str_to_modify = str_to_modify
            self.lines_to_add = lines_to_add
            self.check_added_lines = check_added_lines
            self.lines_to_remove = lines_to_remove
            self.print_file = False
            
            
    def args(self):
        """
        Add and parse arguments from command line
        """
        
        parser = ap.ArgumentParser()
        
        parser.add_argument('-f','--wildpath',help='Path with wildcard to generate list of files',required=True,type=str,metavar='',dest='wildpath')
        parser.add_argument('-b','--backup',help='Create files backup',required=False,default=False,type=bool,metavar='',dest='backup')
        parser.add_argument('-rp','--replace',help='String to be replaced',required=False,default=None,type=str,metavar='',dest='replace')
        parser.add_argument('-m','--modify',help='String to replace with',required=False,default=None,type=str,metavar='',dest='modify')
        parser.add_argument('-a','--add',help='Line to add at the end of file',required=False,default=None,type=str,metavar='',dest='add')
        parser.add_argument('-rm','--remove',help='Line to remove from file',required=False,default=None,type=str,metavar='',dest='remove')
        parser.add_argument('-c','--check',help='Check if added lines are already present',required=False,default=True,type=bool,metavar='',dest='check_added_lines')
        parser.add_argument('-e','--exclude',action='append',help='File to exclude from modification',required=False,default=None,type=str,metavar='',dest='exclude')
        parser.add_argument('-p','--print',help='Print filename',required=False,default=True,type=bool,metavar='',dest='print_file')
        
        args = parser.parse_args()
        
        # update settings
        self.wildpath = args.wildpath
        self.files = glob(os.path.normpath(self.wildpath))
        self.exclude = args.exclude
        self.backup = args.backup
        if args.replace != None and args.modify !=None :
            self.str_to_modify = {args.replace : args.modify}
        else:
            self.str_to_modify = {}
        self.lines_to_add = [args.add] if args.add else []
        self.check_added_lines = args.check_added_lines
        self.lines_to_remove = args.remove if args.remove else []
        self.print_file = args.print_file
        
        return
            
    
    def modify(self):
        for f in self.files:
            if self.print_file:
                print(f)
            change_file(f,back_up_file=self.backup,str_to_modify=self.str_to_modify,lines_to_add=self.lines_to_add,
                        check_added_lines=self.check_added_lines,lines_to_remove=self.lines_to_remove)
        return
        
        
def change_file (input_file , output_file=None, back_up_file = True,
                 str_to_modify = {}, lines_to_add=[], check_added_lines = True, lines_to_remove = []):
    
    ''' function to generate a copy of a file changing a string in specific lines
         In case input and output files are the same a copy of the old file is 
         created with name 'old_{input_file}'
    
     input_file: input file to change
     output_file: output file to generate
                  - Default is 'None' - in this case the same is set equal as the input file
                  - In case output_file and input_file have the same name a copy of 
                    the original input file is saved
     str_to_modify: dictionary of strings to be modified - format: {'old string':'new string'}
     lines_to_add: list of lines to be added at the end of the file: format: ['string 1','string 2']
     check_added_lines: - True - if line is already present is not added
                        - False - line is added indipendently of the others
    ''' 
    origin_file = open(input_file,'r')
    lines_list = origin_file.readlines()
    origin_file.close()
    
    if output_file == None or input_file == output_file:
        output_file = input_file
        if back_up_file == True:
            os.rename(input_file, 'old_' + input_file) 
        else:
            os.remove(input_file)
    
    new_file = open(output_file,'w')
           
    for line in lines_list:        
        rem_line = False
        mod_line = False 
        for l in lines_to_remove:
            if l + '\n' == line:
                rem_line = True
        for string in str_to_modify: 
            target_line = re.findall(string, line)                      
            if target_line != [] and rem_line == False:
                new_file.write(line.replace(string,str_to_modify[string]))
                mod_line = True                  
        if mod_line == False and rem_line == False:
             new_file.write(line)   
         
    for l in lines_to_add:
          if check_added_lines:              
              if l + '\n' not in lines_list:            
                  new_file.write(l + '\n')
          else:
              new_file.write(l + '\n')
    
    new_file.close()
    
    return


# part to execute if script is used directly        
if __name__ == '__main__':
    
    m = ModifyFiles()
    # getting arguments
    m.args()
    # modify file
    m.modify()


