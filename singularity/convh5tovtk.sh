#!/bin/bash


#run h5tovtk to convert hdf5 output from tristan to a VTK format readable by paraview
#h5tovtk part of h5utils available as a deb packagae for ubuntu on https://launchpad.net/ubuntu/bionic/+package/paraview



dir="output-th0ph90sig0p1"

#for (( i=1; i<=9; i++ ))
#do



#for field in "bx" "by" "bz" "dens" "densi" "ex" "ey" "ez" "jx" "jy" "jz" "v3x" "v3xi" "v3y" "v3yi" "v3z" "v3zi"
#do
#	echo $field
#	h5tovtk ${dir}/flds.tot.00${i} -d ${field} -o ${dir}/vtk/flds-${field}.tot.00${i}

#done

#done




for (( i=10; i<=99; i++ ))
do

for field in "bx" "by" "bz" "dens" "densi" "ex" "ey" "ez" "jx" "jy" "jz" "v3x" "v3xi" "v3y" "v3yi" "v3z" "v3zi"
do
	echo $field
	h5tovtk ${dir}/flds.tot.0${i} -d ${field} -o ${dir}/vtk/flds-${field}.tot.0${i}

done
	echo $i
done

for (( i=100; i<=294; i++ ))
do

for field in "bx" "by" "bz" "dens" "densi" "ex" "ey" "ez" "jx" "jy" "jz" "v3x" "v3xi" "v3y" "v3yi" "v3z" "v3zi"
do
	echo $field
	h5tovtk ${dir}/flds.tot.${i} -d ${field} -o ${dir}/vtk/flds-${field}.tot.${i}

done


	echo $i
done
