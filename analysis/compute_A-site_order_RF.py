import numpy as np
import joblib
import ase.io
from pynter import LOCAL_DIR
import os
import ase.io
from tqdm import tqdm
from dscribe.descriptors import SOAP
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
    df_file = df_file.replace(filename_nopath,'df_A-site_' + filename_nopath)


### LOAD ML MODEL

data_path = os.path.join(LOCAL_DIR,'NaBiTi2O6/A-site-order-classifier/models/training/random-forest/')
model = joblib.load(data_path+'A-site_classifier_RF.joblib')
model.verbose = 0


### INIT SOAP

species = ["Na", "Bi", "Ti", "O"]  
soap = SOAP(
    species=species,
    periodic=True,          # Enable for bulk crystals
    r_cut=6.0,              # Cutoff radius
    n_max=8,                # Radial basis functions
    l_max=6,                # Maximum degree of spherical harmonics
    sparse=False,          # Return dense NumPy arrays
)


### COMPUTE A-site ORDER

atoms_list = ase.io.read(filename,index=':')
element_map = {
    1:'Na',
    2:'Ti',
    3:'Bi',
    4:'O'}

keys = ['atoms','probabilities']
indexes = []
data = {k:[] for k in keys}

for idx,atoms in tqdm(enumerate(atoms_list), total=len(atoms_list)//structure_interval, desc="Computing A-site order"):
    if idx % structure_interval == 0:
        indexes.append(idx)
        # Assign types from dump file
        # atom_types = atoms.get_array('numbers') # not needed if dump file is written with elements
        # atoms.set_chemical_symbols([element_map[t] for t in atom_types])
        # create SOAP
        Asite_indices = [i for i, atom in enumerate(atoms) if atom.symbol in ['Na', 'Bi']]
        X = soap.create(system=atoms,centers=Asite_indices,n_jobs=-1)  
        data['atoms'].append(atoms)
        probabilities = model.predict_proba(X)
        data['probabilities'].append(probabilities)


### COMPUTE A-site total fractions

df = pd.DataFrame(data,index=indexes)
labels = [f'M{i}' for i in range(1,7)]
number_of_A_sites = len(Asite_indices)
for idx,label in enumerate(labels):
    df[f'{label}_fraction'] = df.apply(lambda row: np.sum(row['probabilities'][:,idx])/number_of_A_sites,axis=1)
    
### SAVE FILE
df.to_pickle(df_file)
