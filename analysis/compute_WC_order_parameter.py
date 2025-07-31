#!/usr/bin/env python

import sys
import ase.io
from tqdm import tqdm
import pandas as pd
import argparse

### PARSE ARGS

parser = argparse.ArgumentParser(description='Compute A-site order with SOAP and RF model')
parser.add_argument('dump_file', help='Path to the dump file')
parser.add_argument('--interval', '-i', type=int, default=1,help='Compute every n structures (default: 1)')
parser.add_argument('--outfile', '-o', type=int,default=None,help='Output filename ')
args = parser.parse_args()

filename = args.dump_file
structure_interval = args.interval
if args.outfile:
    df_file = args.outfile
else:
    df_file = filename.replace('dump','pkl')
    filename_nopath = df_file.split('/')[-1]
    df_file = df_file.replace(filename_nopath,'df_WC_order_param_' + filename_nopath)

## Compute WC order parameter

def get_WC_order_parameter(
                        atoms,
                        A_symbols,
                        X_symbols,
                        neighbors_cutoff):
    """
    WC_order_parameter = 1 - < P_{A-X} > / C_X
    """
    from pymatgen.core.structure import Structure
    structure = Structure.from_ase_atoms(atoms)
    total_P_AX = 0
    n_A_sites = 0
    n_X_sites = 0
    for site in structure:
        if site.specie.symbol in A_symbols:
            n_A_sites += 1
            neighbors_list = structure.get_neighbors(site,r=neighbors_cutoff)
            n_X_neighbors = 0
            n_total_neighbors = 0
            for neighbor in neighbors_list:
                if neighbor.specie.symbol in X_symbols:
                    n_X_neighbors += 1
                    n_total_neighbors += 1
                elif neighbor.specie.symbol in A_symbols:
                    n_total_neighbors += 1
            total_P_AX += n_X_neighbors / n_total_neighbors
        elif site.specie.symbol in X_symbols:
            n_X_sites += 1

    if n_A_sites == 0 or n_X_sites == 0:
        raise ValueError("No A or X sites found to compute order parameter.")

    C_X = n_X_sites / (n_A_sites + n_X_sites)
    avg_P_AX = total_P_AX / n_A_sites
    alpha = 1 - avg_P_AX / C_X 
    return alpha

### ITERATE OVER DUMP STRUCTURES

atoms_list = ase.io.read(filename,index=':')
data = []
indexes = []
for idx,atoms in tqdm(enumerate(atoms_list),total=len(atoms_list)//structure_interval,desc='Computing WC order parameter'):
    if idx % structure_interval == 0:
        indexes.append(idx)
        WC = get_WC_order_parameter(atoms,['Sr'],['Na','Bi'],8)
        data.append({
            'atoms':atoms,
            'WC_order_param':WC
        })

### SAVE FILE

df = pd.DataFrame(data,index=indexes)
df.to_pickle(df_file)
