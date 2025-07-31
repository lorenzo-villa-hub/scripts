#!/usr/bin/env python

from mpi4py import MPI
import pandas as pd
import pickle
from ase.constraints import UnitCellFilter
from ase.optimize import LBFGS
from pyace import PyACECalculator
from tqdm import tqdm
import os

# MPI setup
comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

# Load DataFrame on root
if rank == 0:
    df = pd.read_pickle("input_dataframe.pkl")
else:
    df = None

# Broadcast the entire DataFrame
df = comm.bcast(df, root=0)

# Distribute indices for each process
indices = list(range(rank, len(df), size))
local_rows = df.iloc[indices]

# Relax atoms with local progress bar
relaxed_atoms = []
for _, row in tqdm(local_rows.iterrows(), total=len(local_rows), desc=f"Rank {rank}", position=rank):
    atoms = row['atoms'].copy()
    atoms.calc = PyACECalculator(os.getenv("HOME")+'/potentials/ACE/NBTST_v3.yaml')  # Update path
    ucf = UnitCellFilter(atoms)
    LBFGS(ucf, logfile=None).run(fmax=0.05)
    atoms.calc = None
    relaxed_atoms.append(atoms)

# Gather relaxed atoms and indices on root
gathered_atoms = comm.gather(relaxed_atoms, root=0)
gathered_indices = comm.gather(indices, root=0)

# Root process integrates results and saves updated DataFrame
if rank == 0:
    all_atoms = [a for sublist in gathered_atoms for a in sublist]
    all_indices = [i for sublist in gathered_indices for i in sublist]
    df.loc[all_indices, 'atoms_relax'] = all_atoms
    df.to_pickle("relaxed_dataframe.pkl")
    print("Relaxation complete. Saved to 'relaxed_dataframe.pkl'.")
