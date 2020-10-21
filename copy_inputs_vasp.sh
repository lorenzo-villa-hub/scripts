#!/bin/bash

DIR=$1

find . -type f \( -name "INCAR" -o -name "KPOINTS" -o -name "POSCAR" -o -name "POTCAR" -o -name "job.sh" \) -exec cp --parents -r {} $DIR \;
