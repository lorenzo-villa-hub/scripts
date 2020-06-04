#!/usr/bin/env python

import os
from glob import glob
import sys
import argparse as ap
from pynter.tools.change_file import change_file

'''
Usage: modify_input.py [files] [string_to_remove] [string_to_add] 
'''

parser = ap.ArgumentParser()

parser.add_argument('-b','--backup',action='store_true',help='Create backup file, default is False',required=False,default=False,dest='back_up_file')
parser.add_argument('-f','--files',help='Path to files to modify (can take wildcard)',required=True,type=str,metavar='',dest='files')
parser.add_argument('-r','--remove',help='String to remove',required=False,default='',type=str,metavar='',dest='remove')
parser.add_argument('-a','--add',help='String to add',required=False,default='',type=str,metavar='',dest='add')
parser.add_argument('-la','--lines-to-add',action='append',help='List of lines to add',required=False,default=[],type=str,metavar='',dest='lines_to_add')
parser.add_argument('-lr','--lines-to-remove',action='append',help='List of lines to remove',required=False,default=[],type=str,metavar='',dest='lines_to_remove')
parser.add_argument('-c','--check-added-lines',action='store_true',help='Check if lines to add are already present',required=False,default=False,dest='check_added_lines')

args = parser.parse_args()


files = args.files
old_string = args.remove
new_string = args.add

list_files = glob(os.path.normpath(files))

for file in list_files:
 	print('modified file: ' + file)
 	change_file(file,str_to_modify={old_string:new_string},back_up_file=args.back_up_file,
              lines_to_add=args.lines_to_add, check_added_lines=args.check_added_lines, lines_to_remove=args.lines_to_remove)

