#!/usr/bin/env python
# coding: utf-8

# In[1]:

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.colors import LogNorm
import seaborn as sns
from pyace import PyACECalculator
from pathlib import Path
import pandas as pd
import ase.db
from ase.optimize import BFGS
import numpy as np
import sys
import argparse

from ase.optimize import BFGS
from ase.filters import UnitCellFilter
from pymatgen.core.composition import Composition
from ase.eos import calculate_eos, EquationOfState
from pymatgen.core.structure import Structure




# ---------------- Training set volume and forces -----------------------------

def training_dataset_volume_and_forces():

    print('Training dataset energy and forces')

    info_df = pd.read_pickle(fit_path / 'fitting_data_info.pckl.gzip',compression='gzip')

    info_df["volume_per_atom"] = [atoms.get_volume() / n for atoms, n in zip(info_df["ase_atoms"], info_df["NUMBER_OF_ATOMS"])]
    info_df["F_max_atom"] = [np.linalg.norm(f, axis=1).max() for f in info_df["forces"]]


    def plot_energy_vs_volume_and_force(df, title="Training Set"):

        fig, axes = plt.subplots(
            1, 2, figsize=(10, 4.5),
            constrained_layout=True,
            sharey=True
        )

        xbins, ybins = 75, 75

        # ---------------------------
        # Panel 1: Volume/atom vs E/atom
        # ---------------------------
        x1 = df["volume_per_atom"].to_numpy()
        y1 = df["energy_corrected_per_atom"].to_numpy()

        mask1 = np.isfinite(x1) & np.isfinite(y1)
        x1, y1 = x1[mask1], y1[mask1]

        xlim1 = (5,25) #(np.nanmin(x1), np.nanmax(x1))
        ylim  = (np.nanmin(y1), np.nanmax(y1))

        H1, xedges1, yedges1 = np.histogram2d(
            x1, y1,
            bins=[xbins, ybins],
            range=[xlim1, ylim]
        )

        # ---------------------------
        # Panel 2: Fmax vs E/atom
        # ---------------------------
        x2 = df["F_max_atom"].to_numpy()
        y2 = df["energy_corrected_per_atom"].to_numpy()

        mask2 = np.isfinite(x2) & np.isfinite(y2)
        x2, y2 = x2[mask2], y2[mask2]

        xlim2 = (0,100)#(np.nanmin(x2), np.nanmax(x2))

        H2, xedges2, yedges2 = np.histogram2d(
            x2, y2,
            bins=[xbins, ybins],
            range=[xlim2, ylim]
        )

        # -------- shared normalization --------
        vmax = max(H1.max(), H2.max(), 1)
        norm = LogNorm(vmin=1, vmax=vmax)

        # -------- plotting --------
        mesh1 = axes[0].pcolormesh(
            xedges1, yedges1, H1.T,
            norm=norm,
            cmap="plasma",
            shading="auto"
        )
        axes[0].set_xlabel(r"Atomic Volume ($\mathrm{\AA}^3$/atom)")
        axes[0].set_ylabel(r"$E_{\mathrm{DFT}}$ (eV/atom)")
        axes[0].set_xlim(*xlim1)
        axes[0].set_ylim(*ylim)

        mesh2 = axes[1].pcolormesh(
            xedges2, yedges2, H2.T,
            norm=norm,
            cmap="plasma",
            shading="auto"
        )
        axes[1].set_xlabel(r"Max force (eV/$\mathrm{\AA}$)")
        axes[1].set_xlim(*xlim2)

        # Shared colorbar
        cbar = fig.colorbar(mesh1, ax=axes, location="right")
        cbar.set_label("# Structures")

        fig.suptitle(title)

    plot_energy_vs_volume_and_force(info_df, "Training Set")
    return plt.gcf()





# # ---------------- DFT vs ACE training and testing ------------------------

def parity_plots():

    print('Training and testing parity plots')

    train_df = pd.read_pickle(fit_path / 'train_pred.pckl.gzip',compression='gzip')
    test_df = pd.read_pickle(fit_path / 'test_pred.pckl.gzip',compression='gzip')

    bins = 75
    norm = norm = colors.LogNorm(vmin=1, vmax=1000)

    def extract_force_components(series):
        return np.concatenate([f.reshape(-1) for f in series])

    def plot_energy_force_parity(df, title="Dataset"):

        fig, axes = plt.subplots(1, 2, figsize=(12, 5))

        cmap = "plasma" 

        # -----------------------
        # ENERGY
        # -----------------------
        dft_E = df["energy_corrected_per_atom"].values
        ace_E = df["energy_pred"].values / df["NUMBER_OF_ATOMS"].values

        hE = axes[0].hist2d(
            dft_E,
            ace_E,
            bins=bins,
            norm=norm,
            cmap=cmap
        )

        lim_min = min(dft_E.min(), ace_E.min())
        lim_max = max(dft_E.max(), ace_E.max())
        axes[0].plot([lim_min, lim_max], [lim_min, lim_max], color="black", linewidth=1)

        axes[0].set_aspect("equal", adjustable="box")
        axes[0].set_xlabel("$E_{DFT}$ (eV)")
        axes[0].set_ylabel("$E_{ACE}$ (eV)")
        axes[0].set_title("Energy")

        rmse_E = np.sqrt(np.mean((ace_E - dft_E)**2))
        axes[0].text(
            0.05, 0.95,
            f"RMSE = {rmse_E:.5f} eV/atom",
            transform=axes[0].transAxes,
            verticalalignment="top"
        )

        fig.colorbar(hE[3], ax=axes[0], label="# Structures")

        # -----------------------
        # FORCES
        # -----------------------
        dft_F = extract_force_components(df["forces"])
        ace_F = extract_force_components(df["forces_pred"])

        hF = axes[1].hist2d(
            dft_F,
            ace_F,
            bins=bins,
            norm=norm,
            cmap=cmap
        )

        lim_min = min(dft_F.min(), ace_F.min())
        lim_max = max(dft_F.max(), ace_F.max())
        axes[1].plot([lim_min, lim_max], [lim_min, lim_max], color="black", linewidth=1)

        axes[1].set_aspect("equal", adjustable="box")
        axes[1].set_xlabel("DFT forces (eV/Å)")
        axes[1].set_ylabel("ACE forces (eV/Å)")
        axes[1].set_title("Forces")

        rmse_F = np.sqrt(np.mean((ace_F - dft_F)**2))
        axes[1].text(
            0.05, 0.95,
            f"RMSE = {rmse_F:.4f} eV/Å",
            transform=axes[1].transAxes,
            verticalalignment="top"
        )

        fig.colorbar(hF[3], ax=axes[1], label="# Structures")

        fig.suptitle(title)
        plt.tight_layout()


    plot_energy_force_parity(train_df, "Training Set")
    fig1 = plt.gcf()
    plot_energy_force_parity(test_df,  "Testing Set")
    fig2 = plt.gcf()
    return fig1, fig2




# #-------------- Test bulk structures ------------------------------

def test_bulk_structures(train_df):

    print('Test bulk structures')

    def get_energy_diff(row):
        return (row['energy_corrected'] - row['energy_pred']) / row['NUMBER_OF_ATOMS'] 

    def filter(row):
        return phase in row.path and 'active-learning' not in row.path

    data = {}
    for phase in ['Bi2O3','Na2O','Pm3m-SG221','R3-SG146','P4bm-SG100','Cmm2-SG35','NaTiO3','SrO','SrTiO3','TiO2']:    
        data[phase] = []
        for i,row in train_df.iterrows():
            if phase in row['path'] and 'active-learning' not in row['path']:
                data[phase].append(get_energy_diff(row))


    from pynter.tools.format import format_composition

    #plt.rcParams['font.size']= 18

    NBT_labels = {
        'R3-SG146':'NBT (R)',
        'P4bm-SG100': 'NBT (T)',
        'Cmm2-SG35': 'NBT (O)',
        'Pm3m-SG221': 'NBT (C)'
    }

    plt.figure(figsize=(8,6))
    count = 0
    for phase, energy_differences in data.items():
        count += len(energy_differences)
        if 'SG' in phase:
            label = NBT_labels[phase]
        else:
            label = format_composition(phase)
        plt.hist(energy_differences,bins=50,range=(-0.05,0.05),density=True,alpha=0.6, label=label)

    plt.ylim(0,200)
    #plt.yscale('log')
    plt.legend()
    plt.xlabel('$\Delta E$ (eV/atom)')
    plt.ylabel('Count (a.u.)')
    ax = plt.gca()
    ax.legend(loc='center left', bbox_to_anchor=(1.05, 0.5))
    ax.set_yticks([])
    plt.tight_layout()
    #plt.show()
    return plt.gcf()





# # ---- Test A-site hierarchy --------------------------------------------

def test_A_site_hierarchy_R3c(dft_db):
    print('Test rhombo A-site hierarchy')

    # Reference energy of isolated atoms in DFT
    energy_shift_per_atom = {'Bi': -0.0416272,
                            'Na': -0.026782,
                            'O': -0.05054281, 
                            'Sr': -0.0301307, 
                            'Ti': -1.15332708}

    def filter(row):
        return 'tunica' in row.path
    data = []
    for row in dft_db.select(filter=filter):
        label = row.path.split('/')[-2]
        atoms = row.toatoms()
        n_atoms = len(atoms)
        energy_dft = atoms.get_potential_energy()
        atoms.calc = calc
        energy_corr = -1* sum([n*energy_shift_per_atom[el.symbol] for el,n in Composition(atoms.get_chemical_formula()).items()])
        energy = atoms.get_potential_energy() + energy_corr
        
        atoms_relaxed = atoms.copy()
        atoms_relaxed.calc = calc
        ucf = UnitCellFilter(atoms=atoms_relaxed) 
        BFGS(ucf,logfile='ase_relax_log.txt').run(fmax=0.05,steps=200)
        energy_relaxed = atoms.get_potential_energy() + energy_corr
        data.append({
            'label':label,
            'atoms':atoms,
            'atoms_relaxed':atoms_relaxed,
            'energy_pa_DFT':None,
            'energy_pa_ACE':energy/n_atoms,
            'energy_pa_relaxed_DFT':energy_dft/n_atoms,
            'energy_pa_relaxed_ACE':energy_relaxed/n_atoms
        })

    df = pd.DataFrame(data)
    #df['energy_pa_ACE-DFT'] = df.apply(lambda row: row['energy_pa_ACE'] - row['energy_pa_DFT'],axis=1)
    df['energy_pa_relaxed_ACE-DFT'] = df.apply(lambda row: row['energy_pa_relaxed_ACE'] - row['energy_pa_relaxed_DFT'],axis=1)

    def energy_diff_meV(row,method='ACE'):
        energy_111 = df[df['label']=='111'][f'energy_pa_relaxed_{method}']
        return (row[f'energy_pa_relaxed_{method}'] - energy_111) * 1000 

    df['energy_diff_meV_ACE'] = df.apply(lambda row: energy_diff_meV(row,method='ACE'),axis=1)
    df['energy_diff_meV_DFT'] = df.apply(lambda row: energy_diff_meV(row,method='DFT'),axis=1)

    mapping = {
        '111':'$111$',
        '110':'$110$',
        '11_01':'$(11-01)_{xy}$',
        '001':'$001$',
        '10_01_1':'$(10-01)_z$ ',
        '10_01_2':"$(10-01)_z'$ ",
        '3_1':'all$3$+$1$'}


    labels = [l for l in df['label']]
    ax = df.plot(x='label',y=['energy_diff_meV_ACE','energy_diff_meV_DFT'],kind='bar',rot=0,figsize=(8,6))
    ax.set_xticklabels([mapping[tick.get_text()] for tick in ax.get_xticklabels()]);
    ax.set_xlabel('A-site order');
    ax.set_ylabel('Energy (meV/atom)')
    ax.legend(['ACE','DFT'])
    plt.title('R3c phase')
    plt.tight_layout()

    return plt.gcf()




### CUBIC Pm3-m


def test_A_site_hierarchy_Pm3m(dft_db):

    print('Test cubic A-site hierarchy')

    # Reference energy of isolated atoms in DFT
    energy_shift_per_atom = {'Bi': -0.0416272,
                            'Na': -0.026782,
                            'O': -0.05054281, 
                            'Sr': -0.0301307, 
                            'Ti': -1.15332708}

    def filter(row):
        return 'Pm3m-SG221/A-site-order' in row.path
    data = []
    for row in dft_db.select(filter=filter):
        label = row.path.split('/')[-2]
        atoms = row.toatoms()
        n_atoms = len(atoms)
        energy_dft = atoms.get_potential_energy()
        atoms.calc = calc
        energy_corr = -1* sum([n*energy_shift_per_atom[el.symbol] for el,n in Composition(atoms.get_chemical_formula()).items()])
        energy = atoms.get_potential_energy() + energy_corr
        
        atoms_relaxed = atoms.copy()
        atoms_relaxed.calc = calc
        ucf = UnitCellFilter(atoms=atoms_relaxed) 
        BFGS(ucf,logfile='ase_relax_log.txt').run(fmax=0.05,steps=200)
        energy_relaxed = atoms.get_potential_energy() + energy_corr
        data.append({
            'label':label,
            'atoms':atoms,
            'atoms_relaxed':atoms_relaxed,
            'energy_pa_DFT':None,
            'energy_pa_ACE':energy/n_atoms,
            'energy_pa_relaxed_DFT':energy_dft/n_atoms,
            'energy_pa_relaxed_ACE':energy_relaxed/n_atoms
        })


    df = pd.DataFrame(data)

    #df['energy_pa_ACE-DFT'] = df.apply(lambda row: row['energy_pa_ACE'] - row['energy_pa_DFT'],axis=1)
    df['energy_pa_relaxed_ACE-DFT'] = df.apply(lambda row: row['energy_pa_relaxed_ACE'] - row['energy_pa_relaxed_DFT'],axis=1)

    def energy_diff_meV(row,method='ACE'):
        energy_111 = df[df['label']=='M1'][f'energy_pa_relaxed_{method}']
        return (row[f'energy_pa_relaxed_{method}'] - energy_111) * 1000 

    df['energy_diff_meV_ACE'] = df.apply(lambda row: energy_diff_meV(row,method='ACE'),axis=1)
    df['energy_diff_meV_DFT'] = df.apply(lambda row: energy_diff_meV(row,method='DFT'),axis=1)

    mapping = {
        'M1':'$111$',
        'M2':'$110$',
        'M3':'$(11-01)_{xy}$',
        'M4':'$001$',
        'M5':'$(10-01)_z$ ',
        'M6':'all$3$+$1$'}


    labels = [l for l in df['label']]
    ax = df.plot(x='label',y=['energy_diff_meV_ACE','energy_diff_meV_DFT'],kind='bar',rot=0,figsize=(8,6))
    ax.set_xticklabels([mapping[tick.get_text()] for tick in ax.get_xticklabels()]);
    ax.set_xlabel('A-site order');
    ax.set_ylabel('Energy (meV/atom)')
    ax.legend(['ACE','DFT'])
    plt.title('Pm3-m phase')
    plt.tight_layout()
    
    return plt.gcf()





# # ------------ Test energy vs volume --------------------------------------


def test_energy_vs_volume(dft_db):

    print('Test equation of state')

    labels = [f'M{i}' for i in range(1,7)]
    def filter(row):
        return 'volume' in row.path
    data = []
    for row in dft_db.select(filter=filter):
        atoms_dft = row.toatoms()
        label = row.path.split('/')[-2]
        eps = float(row.path.split('/')[-1].strip('eps'))
        d = {
            'label':label,
            'method':'DFT',
            'eps':eps,
            'volume':atoms_dft.get_volume(),
            'energy':row.energy
        }
        data.append(d)
    df = pd.DataFrame(data)


    # two stacked axes sharing x (broken y-axis)
    fig, (ax_top, ax_bottom) = plt.subplots(
        2, 1, sharex=True, figsize=(8,10),
        gridspec_kw={'height_ratios':[1,1], 'hspace':0.05},
        constrained_layout=True
    )

    colors = matplotlib.color_sequences['tab10']
    label_mapping = {
            'M1':'$111$',
            'M2':'$110$',
            'M3':'$(11-01)_{xy}$',
            'M4':'$001$',
            'M5':'$(10-01)_z$',
            'M6':'all$3$+$1$'}


    for idx,label in enumerate(labels):
        df_label = df[df['label'] == label]
        volumes = df_label['volume'].to_list()
        energies = df_label['energy'].to_list()
        eos = EquationOfState(volumes=volumes,energies=energies)
        v0, e0, B = eos.fit()
        plotdata = eos.getplotdata()
        x,y,v,e = plotdata[4:8]

        ax_top.plot(x,y,label=label_mapping[label],color=colors[idx],lw=3)
        ax_top.scatter(v,e,color=colors[idx],s=40)

        def filter(row):
            return 'volume' in row.path and label in row.path
        for row in dft_db.select(filter=filter):
            if label != row.path.split('/')[-2]:
                raise ValueError('Label mismatch!')
            eps = float(row.path.split('/')[-1].strip('eps'))
            if eps == 1:
                atoms = row.toatoms()
                atoms.calc = calc
                eos = calculate_eos(atoms=atoms,npoints=9)
                v0, e0, B = eos.fit()
                plotdata = eos.getplotdata()
                x,y,v,e = plotdata[4:8]

                ax_bottom.plot(x,y,ls='--',color=colors[idx],lw=3)
                ax_bottom.scatter(v,e,color=colors[idx],s=40)

    # set the two y ranges 
    ax_top.set_ylim(-300,-298)
    ax_bottom.set_ylim(-311,-309)

    # hide touching spines
    ax_top.spines.bottom.set_visible(False)
    ax_bottom.spines.top.set_visible(False)
    ax_top.tick_params(labeltop=False)
    ax_bottom.xaxis.tick_bottom()

    # draw the // markers
    d = 0.5
    kwargs = dict(marker=[(-1,-d),(1,d)], markersize=12, linestyle="none",
                color='k', mec='k', mew=1, clip_on=False)
    ax_top.plot([0,1],[0,0], transform=ax_top.transAxes, **kwargs)
    ax_bottom.plot([0,1],[1,1], transform=ax_bottom.transAxes, **kwargs)

    ax_bottom.set_xlabel('Volume (Å$^3$)')
    ax_bottom.set_ylabel('Energy (eV)')

    # Legend 1: structure labels
    ax_top.legend(title='A-site order',bbox_to_anchor=(1,0.8))

    # Legend 2: line types
    from matplotlib.lines import Line2D
    legend_lines = [Line2D([0],[0], color='k', ls='-'),
                    Line2D([0],[0], color='k', ls='--')]
    ax_bottom.legend(legend_lines, ['DFT','ACE'], title='Method', bbox_to_anchor=(1.3,1))

    return plt.gcf()





# #  ------------------ Test defect formation energies --------------------------------

def test_defect_formation_energies(dft_db):

    print('Test defect formation energies in 111')

    # CHEMICAL POTENTIALS
    mu_refs_DFT = {}
    mu_refs_ACE = {}

    def filter(row):
        return 'ElementalPhases' in row.path
    for row in dft_db.select(filter=filter):
        atoms_dft = row.toatoms()
        el = atoms_dft.symbols[0]
        mu_refs_DFT[el] = atoms_dft.get_potential_energy()/len(atoms_dft)
        atoms = atoms_dft.copy()
        atoms.calc = calc 
        mu_refs_ACE[el] = atoms.get_potential_energy()/len(atoms)

    print(mu_refs_ACE)


    from defermi.defects import get_delta_atoms_from_comp

    # Reference energy of isolated atoms in DFT
    energy_shift_per_atom = {'Bi': -0.0416272,
                            'Na': -0.026782,
                            'O': -0.05054281, 
                            'Sr': -0.0301307, 
                            'Ti': -1.15332708}

    data = []
    def bulk_filter(row):
        return 'R3-SG146/Bulk/2x2x2-supercell' in row.path
    atoms_bulk = [row for row in dft_db.select(filter=bulk_filter)][0].toatoms() 
    energy_bulk_DFT = atoms_bulk.get_potential_energy()
    
    atoms_bulk.calc = calc
    energy_bulk_ACE = atoms_bulk.get_potential_energy()

    bulk_composition = Structure.from_ase_atoms(atoms_bulk).composition

    # DFT bulk
    data.append({
    'method':'DFT',
    'type':'relaxed',
    'atoms':atoms_bulk,
    'composition':bulk_composition,
    'defect_name':'Bulk',
    'charge':0,
    'energy':energy_bulk_DFT,
    })

    data.append({
    'method':'ACE',
    'type':'relaxed',
    'atoms':atoms_bulk,
    'composition':bulk_composition,
    'defect_name':'Bulk',
    'charge':0,
    'energy':energy_bulk_ACE
    })


    def filter_step1(row):
        return 'R3-SG146/Defects' in row.path and '2-PBE-OPT' in row.path
    def filter_step2(row):
        return 'R3-SG146/Defects' in row.path and '2-PBE-OPT' in row.path
    groups_added = []

    all_converged = True
    for row in dft_db.select(filter=filter_step1):
        group = row.path.split('/')[-3]
        charge = int(row.path.split('/')[-2].strip('q'))
        initial_atoms_dft = row.toatoms()
        natoms = len(initial_atoms_dft)
        composition = Structure.from_ase_atoms(initial_atoms_dft).composition

        energy_DFT_ideal = initial_atoms_dft.get_potential_energy()
        data.append({
            'method':'DFT',
            'type':'ideal',
            'atoms':initial_atoms_dft,
            'composition': composition,
            'defect_name':group,
            'charge':charge,
            'energy':energy_DFT_ideal
        })

        step1_path = row.path
        def filter(row):
            return step1_path.replace('1-PBE-SCF','2-PBE-OPT') == row.path
        final_atoms_dft = [row for row in dft_db.select(filter=filter)][0].toatoms()
        data.append({
        'method':'DFT',
        'type':'relaxed',
        'atoms':final_atoms_dft,
        'composition': composition,
        'defect_name':group,
        'charge':charge,
        'energy':final_atoms_dft.get_potential_energy()
        })

        if group not in groups_added:
            groups_added.append(group)
            atoms = initial_atoms_dft.copy()
            atoms.calc = calc

            data.append({
                'method':'ACE',
                'type':'ideal',
                'atoms':atoms,
                'composition':composition,
                'defect_name':group,
                'charge':0, # using 0 instead of None because defects_df forces all items in column to be float in NaN is present
                'energy':atoms.get_potential_energy()
            })
            
            
            converged = BFGS(atoms=atoms,logfile='ase_relax_log.txt').run(fmax=0.05,steps=200); # relax with ACE
            if not converged:
                all_converged = False
            data.append({
                'method':'ACE',
                'type':'relaxed',
                'atoms':atoms,
                'composition':composition,
                'defect_name':group,
                'charge':0,
                'energy':atoms.get_potential_energy()
            })

    defects_df = pd.DataFrame(data)

    def get_energy_corr(row):
        energy = row['energy']
        if row['method'] == 'ACE':
            energy -= sum([n*energy_shift_per_atom[el.symbol] for el,n in row['composition'].items()])
        return energy
        
    defects_df['energy_corr'] = defects_df.apply(lambda row: get_energy_corr(row),axis=1)

    defects_df['energy_DF-BK'] = defects_df.apply(lambda row: row['energy_corr'] - 
                                        defects_df.loc[(defects_df['method']==row['method']) & (defects_df['defect_name']=='Bulk'), 'energy_corr'].values[0],axis=1)

    band_gap = 2.7666
    vbm = 3.3297
    def get_formation_energy(row):
        energy_diff = row['energy_DF-BK'] 
        if row['method'] == 'DFT':
            charge = row['charge']
            mu_refs = mu_refs_DFT
        elif row['method'] == 'ACE':
            charge = 0
            mu_refs = mu_refs_ACE
        return energy_diff - sum([n*mu_refs[el.symbol] for el,n in get_delta_atoms_from_comp(row['composition'],bulk_composition).items()]) + charge*vbm

    defects_df['formation_energy'] = defects_df.apply(get_formation_energy,axis=1)


    defects_df['energy_DF-BK_DFT-ACE'] = defects_df.apply(lambda row: row['energy_DF-BK'] - 
                                        defects_df.loc[(defects_df['method']=='ACE') & (defects_df['defect_name']==row['defect_name']) & (defects_df['type']==row['type']), 'energy_DF-BK'].values[0], axis=1)
    defects_df['formation_energy_DFT-ACE'] = defects_df.apply(lambda row: row['formation_energy'] -
                                            defects_df.loc[(defects_df['method']=='ACE') & (defects_df['defect_name']==row['defect_name']) & (defects_df['type']==row['type']), 'formation_energy'].values[0], axis=1)




    from defermi.defects import get_defect_from_string

    sns.set_theme(context='talk',style='whitegrid')

    df_pivot = defects_df.pivot_table(
        index=['charge'],
        columns=['defect_name'],
        values=['formation_energy_DFT-ACE'])
    df_pivot.columns = df_pivot.columns.get_level_values(1)

    labels_dict = {}
    for name in df_pivot.columns:
        if name=='Bulk':
            labels_dict[name]='Bulk'
        else:
            labels_dict[name]= get_defect_from_string(name).symbol


    ## Defects
    columns_to_plot = ['Vac_O','Vac_Na','Sub_Sr_on_Na','Sub_Sr_on_Bi']
    ax = df_pivot.plot(kind='bar',y=columns_to_plot,rot=0,figsize=(6,6))
    ax.legend(labels=[labels_dict[name] for name in columns_to_plot])
    plt.xlabel('Charge')
    plt.ylabel('$ \Delta E_f ^{DFT} -  \Delta E_f ^{ACE}$  (eV)')
    plt.xlim(3,7.2) # indexes not charge values
    #plt.ylim(-6.3,2)
    plt.tight_layout()

    if all_converged:
        return plt.gcf()
    else:
        return None




## ------------- Test A-site disorder ----------------------------------------------


def test_A_site_disordered_structures(dft_db):

    print('Test A-site disordered structures')

    from icet import ClusterSpace
    from icet.tools.structure_generation import occupy_structure_randomly
    from icet.tools.training_set_generation import structure_selection_annealing

    # Create A-site disordered structures
    def filter(row):
        return 'R3c-SG161/energy-volume/M4/eps1.0' in row.path
    primitive_atoms = [row for row in dft_db.select(filter=filter)][0].toatoms()
    cutoffs = [10.0, 6.0, 4.0]
    chemical_symbols = [] 
    for site in primitive_atoms:
        if site.symbol in ['Na','Bi']:
            chemical_symbols.append(['Na','Bi'])
        else:
            chemical_symbols.append([site.symbol])
    cluster_space = ClusterSpace(primitive_atoms, cutoffs=cutoffs, chemical_symbols=chemical_symbols)
    supercell_size = 2
    primitive_atoms_supercell = primitive_atoms.copy().repeat(supercell_size)
    atoms_list = []
    for i in range(10):
        atoms = primitive_atoms_supercell.copy()
        concentration = {'Na':0.5,'Bi':0.5}
        occupy_structure_randomly(atoms, cluster_space, concentration)
        atoms_list.append(atoms)


    # Check E(disorder) - E(001)
    from ase.filters import UnitCellFilter

    hydrostatic_strain = True
    atoms = primitive_atoms_supercell.copy()
    atoms.calc = calc
    ucf = UnitCellFilter(atoms,hydrostatic_strain=hydrostatic_strain)
    BFGS(ucf,logfile='ase_relax_log.txt').run(fmax=0.05,steps=200)
    energy_M4 = atoms.get_potential_energy()/len(atoms)
    atoms_M4 = atoms.copy()

    data = []
    all_converged = True
    for atoms in atoms_list:
        atoms.calc = calc
        energy = atoms.get_potential_energy()/len(atoms)
        ucf = UnitCellFilter(atoms,hydrostatic_strain=hydrostatic_strain)
        converged = BFGS(ucf,logfile='ase_relax_log.txt').run(fmax=0.05,steps=200)
        if not converged:
            all_converged = False
        energy_rel = atoms.get_potential_energy() /len(atoms)

        d = {
            'atoms':atoms.copy(),
            'energy':energy,
            'energy_rel':energy_rel,
            'energy_diff':energy_rel - energy_M4
        }
        data.append(d)

    df = pd.DataFrame(data)

    mean = df["energy_diff"].mean()
    std = df["energy_diff"].std()
    print(f'MEAN energy diff btw disorder and 001 R3c: {mean}')
    print(f'STD energy diff btw disorder and 001 R3c: {std}')

    plt.figure(figsize=(6,6))
    plt.bar(['A-site disorder'], [mean], yerr=[std], capsize=5, width=0.4)
    plt.ylabel('$\Delta E_{001}^{disordered}$ (eV/atom)')
    plt.title(f'$<\Delta E>$ = {round(mean,3)*1000} meV')
    plt.tight_layout()

    if all_converged:
        return plt.gcf()
    else:
        return None






if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Test ACE potential for NBT-ST')

    parser.add_argument('filename', help='ACE potential filename')
    
    # paths
    parser.add_argument('--fit-path', '-fp', type=str,default=None,dest='fit_path',metavar='',help='Path with fitting output')
    parser.add_argument('--db-filename', '-db', type=str, metavar='',
                        default='/nfshome/villa/local-data/NaBiTi2O6/ML-potentials/ACE/testing/databases/testing.db',
                        dest='db_path',help='ASE db filename with custom DFT data')
    parser.add_argument('--report-path', '-rp', type=str,default=None,dest='report_path',metavar='',help='Path to store report')

    # testing functions
    parser.add_argument('--train-set','-V',action='store_false',dest='train_set',help=' Exclude training set distribution')
    parser.add_argument('--parity','-pa',action='store_false',dest='parity',help='Exclude parity plots')
    parser.add_argument('--rhombo','-R',action='store_false',dest='rhombo',help='Exclude A-site R3c hierarchy')
    parser.add_argument('--cubic','-C',action='store_false',dest='cubic',help='Exclude A-site Pm3m hierarchy')
    parser.add_argument('--eos','-EV',action='store_false',dest='eos',help='Exclude equation of state')
    parser.add_argument('--defects','-df',action='store_false',dest='defects',help='Exclude defect formation energies')
    parser.add_argument('--disorder','-ds',action='store_false',dest='disorder',help='Exclude A-site disorder')


    args = parser.parse_args()


    fit_path = Path(args.fit_path) if args.fit_path else Path.cwd()
    calc = PyACECalculator( str(fit_path / args.filename) )

    db_path = args.db_path
    if not Path(db_path).exists():
        raise ValueError('DFT Database path does not exist')
    dft_db = ase.db.connect(db_path)

    report_path = Path(args.report_path) if args.report_path else Path.cwd()

    sns.set_theme(context='talk',style='whitegrid')
    figures = []

    if args.train_set:
        fig = training_dataset_volume_and_forces()
        figures.append(fig)

    if args.parity:
        parity_figs = parity_plots()
        for fig in parity_figs:
            figures.append(fig)

    if args.rhombo:
        fig = test_A_site_hierarchy_R3c(dft_db)
        figures.append(fig)

    if args.cubic:
        fig = test_A_site_hierarchy_Pm3m(dft_db)
        figures.append(fig)

    if args.eos:
        fig = test_energy_vs_volume(dft_db)
        figures.append(fig)
    
    if args.defects:
        fig = test_defect_formation_energies(dft_db)
        if fig:
            figures.append(fig)

    if args.disorder:
        fig = test_A_site_disordered_structures(dft_db)
        if fig:
            figures.append(fig)

    

    # # Save figures

    from matplotlib.backends.backend_pdf import PdfPages

    with PdfPages( str(report_path / "report.pdf") ) as pdf:
        for fig in figures:
            pdf.savefig(fig)   # saves the current figure as a new page
            plt.close(fig)     # optional but recommended

