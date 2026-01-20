#!/usr/bin/env bash
# remove_unwanted_recursive.sh

# List of files to keep
keep=("INCAR" "POSCAR" "POTCAR" "KPOINTS" "job.sh" "vasprun.xml" "LOCPOT" "OUTCAR")

# Turn into a regex pattern for `find`
keep_pattern=$(printf "|%s" "${keep[@]}")
keep_pattern=${keep_pattern:1}   # drop leading |

# Find all files under the current directory
find . -type f | while read -r f; do
    base=$(basename "$f")
    if [[ ! "$base" =~ ^($keep_pattern)$ ]]; then
        echo "Removing $f"
        rm -f -- "$f"
    fi
done

