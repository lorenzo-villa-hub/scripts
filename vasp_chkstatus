#!/bin/bash

if [ "$#" -lt 1 ] ; then
    echo "usage: `basename $0` {<files>}" > /dev/stderr
    echo "  extracts energies and maximum forces from OUTCAR" > /dev/stderr
    exit
fi

files=$@
for outcar in $files ; do
    if [ ! -f $outcar ] ; then
	echo "ERROR: file $outcar does not exist" > /dev/stderr
	exit
    fi
done

echo "# 1st col: iteration"
echo "# 2nd col: number of SCF cycles"
echo "# 3rd col: free energy (eV)"
echo "# 4th col: energy without entropy (eV)"
echo "# 5th col: change in total energy (eV)"
echo "# 6th col: maximum force (meV/A)"
echo "# 7th-9th col: maximum force in x,y,z (meV/A)"
echo "# 10th col: id of atom with maximum force"
echo "# 11th col: average absolute force (meV/A)"
echo "# 12th-17th col: stress tensor (kB)"

Eold=0

for outcar in $files ; do

    nions=`grep "number of ions" $outcar | head -1 | awk '{print $12}'`
    isw=`grep "free  energy" $outcar | wc | awk '{print $1}'`

    echo "# file= $outcar    nions= $nions    isw= $isw"

    if [ ! $isw ] ; then

	echo "ERROR: no finished iteration steps" > /dev/stderr

    else

	for ((i=1;i<=$isw;i++)) ; do

	    ii=`echo $i | awk '{printf("%3d",$1)}'`

	    E=`grep "energy  without entropy" $outcar \
                 | head -$i | tail -1 | awk '{printf("%14.6f\n",$7)}'`
	    F=`grep "free  energy" $outcar \
                 | head -$i | tail -1 | awk '{printf("%14.6f\n",$5)}'`
	    stress=`grep "in kB" $outcar \
                 | head -$i | tail -1 \
                        | awk '{printf(" %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f\n", $3, $4, $5, $6, $7, $8)}'`
	    dE=`echo $E $Eold | awk '{printf("%14.6f\n",$1-$2)}'`
	    Eold=$E
	    str=`echo $i | awk '{printf("Iteration  %5d(",$1)}'`
	    nscf=`grep "$str" $outcar | tail -1 | awk '{printf("%3d",$4)}'`
	    fmax=`grep -m$i -A\`expr $nions + 1\` TOTAL-FORCE $outcar \
                 | tail -$nions \
                 | awk '{
              fx=1e3*$4; fy=1e3*$5; fz=1e3*$6;
              f=sqrt(fx^2+fy^2+fz^2);
              printf("%7.1f  %7.1f %7.1f %7.1f  %3d\n", f, fx, fy, fz, NR)}' \
                 | sort -rgk1 | head -1`
	    favg=`grep -m$i -A\`expr $nions + 1\` TOTAL-FORCE $outcar \
                 | tail -$nions \
                 | awk -v nions=$nions \
                 'function abs(x) {return (x<0?-x:x)}
                 {ftot+=(abs($4)+abs($5)+abs($6))*1e3}
                 END{printf("%7.1f",ftot/nions/3)}'`

	    printf "$ii  $nscf  $F $E $dE $fmax  $favg    $stress\n"

	done

    fi

done
