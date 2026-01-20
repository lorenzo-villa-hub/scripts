#!/usr/bin/env python

import ase.io
from dscribe.descriptors import SOAP
import pandas as pd
from tqdm import tqdm
import joblib 
import numpy as np
import argparse
import os
from pynter import LOCAL_DIR



def compute_A_site_order(atoms_list,structure_interval=1):

    species = ["Na", "Bi"] 
    soap = SOAP(
        species=species,
        periodic=True,          # Enable for bulk crystals
        r_cut=6.0,              # Cutoff radius
        n_max=15,                # Radial basis functions
        l_max=6,                # Maximum degree of spherical harmonics
        sigma=0.4,
        sparse=False)          # Return dense NumPy arrays

    model = joblib.load(os.path.join(LOCAL_DIR,'NaBiTi2O6/A-site-order-classifier/models/training/random-forest/classifier/without-Ti-O/A-site_classifier_RF_wo_Ti-O.joblib'))
    model.verbose = 0

    n_atoms_list = len(atoms_list)
    total = n_atoms_list // structure_interval
    if n_atoms_list % structure_interval != 0:
        total += 1 
    print('Number of structures to compute:%i' %total)

    keys = ['atoms','probabilities']
    indexes = []
    data = {k:[] for k in keys}
    for idx,atoms in tqdm(enumerate(atoms_list), total=n_atoms_list, desc="Computing A-site order"):
        if idx % structure_interval == 0 or idx==(n_atoms_list-1): #include last structure
            indexes.append(idx)
            to_delete = [atom.index for atom in atoms if atom.symbol not in ("Na", "Bi")]
            del atoms[to_delete]
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
        df[f'{label}_fraction'] = df.apply(lambda row: np.mean(row['probabilities'][:,idx]),axis=1)

    return df


def compute_A_site_order_from_dump_file(dump_file,structure_interval=1,nstructures=None):

    print(f'Reading dump file: {dump_file}')
    atoms_list = ase.io.read(dump_file,index=':')
    n_atoms_list = len(atoms_list)
    print('Number of structures in dump file:%i' %n_atoms_list)
    if structure_interval == 1:
        if nstructures:
            if nstructures > n_atoms_list:
                nstructures = n_atoms_list
            else:
                structure_interval = n_atoms_list//nstructures

    return compute_A_site_order(atoms_list,structure_interval)



def plot_A_site_order(df,MC_attempts_per_step=8000):

    import matplotlib.pyplot as plt
    import seaborn as sns
    import pandas as pd
    from matplotlib.lines import Line2D

    sns.set_theme(context='talk',style='whitegrid')

    disordered_fractions = {  # computed with avg on disordered systems with RF model without Ti-O
        'M1': 0.012774884259259258,
        'M2': 0.1022236689814815,
        'M3': 0.32256944444444446,
        'M4': 0.09205005787037038,
        'M5': 0.3423249421296296,
        'M6': 0.12805700231481482}

    mapping = {
        'M1':'$111$',
        'M2':'$110$',
        'M3':'$(11-01)_{xy}$',
        'M4':'$001$',
        'M5':'$(10-01)_z$',
        'M6':'all$3$+$1$'}


    colors = plt.rcParams['axes.prop_cycle'].by_key()['color']

    plt.figure(figsize=(8,6))

    for i in range(1,7):
        data = df[f'M{i}_fraction']
        sns.lineplot(x=MC_attempts_per_step*data.index,y=data.values,label=mapping[f'M{i}'],color=colors[i-1])
        plt.axhline(y=disordered_fractions[f'M{i}'],ls='--',lw=1.6,color=colors[i-1],alpha=0.7)
    plt.ylabel('A-site order fraction')
    plt.xlabel('MC attempts')
    handles, labels = plt.gca().get_legend_handles_labels()
    handles.append(Line2D([0], [0], color="grey", lw=2,ls='--',alpha=0.6))
    labels.append("random")

    plt.legend(handles, labels,loc='upper right',fontsize=12,bbox_to_anchor=(1.4,0.9))
    plt.ticklabel_format(axis='x',style='sci',scilimits=[0,1],useMathText=True)
    plt.ylim(-0.02,1.02)
    plt.tight_layout()
    #plt.show()
    return plt


if __name__ == '__main__':

### PARSE ARGS

    parser = argparse.ArgumentParser(description='Compute A-site order with SOAP and RF model')
    parser.add_argument('dump_file', help='Path to the dump file')
    parser.add_argument('--interval', '-i', type=int, default=1,help='Compute every n structures (default: 1)')
    parser.add_argument('--nstructures','-n',type=int, default=None,help='Number of structures to compute (plus last structure eventually)')
    parser.add_argument('--outfile', '-o', type=int,default=None,help='Output filename ')
    args = parser.parse_args()

    filename = args.dump_file
    structure_interval = args.interval
    nstructures = args.nstructures
    if args.outfile:
        df_file = args.outfile
    else:
        df_file = filename.replace('dump','pkl')
        filename_nopath = df_file.split('/')[-1]
        df_file = df_file.replace(filename_nopath,'df_A-site_' + filename_nopath)

    df = compute_A_site_order_from_dump_file(
                                            filename,
                                            structure_interval=structure_interval,
                                            nstructures=nstructures
                                            )
    df.to_pickle(df_file)

    plt = plot_A_site_order(df=df,MC_attempts_per_step=8000)
    plt.savefig('A-site_order.pdf',bbox_inches='tight')
