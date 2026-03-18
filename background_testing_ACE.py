
import time
import subprocess
import sys
import shutil
from pathlib import Path
import os


def run_every(interval, function, *args, **kwargs):
    while True:
        start = time.time()
        function(*args, **kwargs)        
        elapsed = time.time() - start
        time.sleep(max(0, interval - elapsed))


def run_backup_and_testing(
                        script_filename,
                        iteration_interval=100):
    
    original_wdir = Path.cwd().resolve()
    base_path = Path("testing")
    iter = False
    if Path('log.txt').exists():
        with open('log.txt','r') as logfile:
            lines = logfile.readlines()
        lines = list(reversed(lines))
        for line in lines:
            if 'Iteration   #' in line:
                iter = int(line.split('#')[1].split(' ')[0])
                potential_filename = f'interim_potential_iter{str(iter).zfill(3)}.yaml'
                break
            elif 'Final potential is saved to' in line:
                print('Fitting completed, stopping the program...')
                sys.exit(0) # stop the program if fitting is done

    if iter and iter % iteration_interval == 0:
        path = base_path / f'iter_{str(iter).zfill(3)}'
        if not path.exists():
            path.mkdir(parents=True, exist_ok=True)
            with open(path / "stdout.txt", "w") as out_file, open(path / "stderr.txt", "w") as err_file:
                # copy potential
                err_file.write(f'Iteration #{iter} \n')
                err_file.write(f'Copying interim potential as: {str(path / potential_filename)}')
                shutil.copy("interim_potential_0.yaml", path / potential_filename)

                # run testing
                subprocess.run([
                            sys.executable,
                            script_filename,
                             'interim_potential_0.yaml',
                            '--report-path',str(path),
                            '-V',
                            '-pa'
                            ],
                    stdout=out_file,
                    stderr=err_file,
                    text=True)


if __name__ == '__main__':

    run_every(interval=10,function=run_backup_and_testing,script_filename='/nfshome/villa/scripts/testing_NBT_ACE.py')
